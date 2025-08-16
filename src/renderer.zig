const std = @import("std");
const SDL = @import("SDL2");
const gl = @import("zgl");

const gpu_types = @import("gpu_types.zig");

const Position = struct {
    x: gl.Short,
    y: gl.Short,

    const Self = @This();

    pub fn from_gpu_xy(xy: gpu_types.Xy) Self {
        return .{
            .x = @bitCast(xy.x),
            .y = @bitCast(xy.y),
        };
    }
};

const Color = struct {
    r: gl.UByte,
    g: gl.UByte,
    b: gl.UByte,

    const Self = @This();

    pub fn from_gpu_color(color: gpu_types.Color) Self {
        return .{
            .r = color.red,
            .g = color.green,
            .b = color.blue,
        };
    }
};

pub fn Buffer(comptime T: type) type {
    return struct {
        const VERTEX_BUFFER_LEN: usize = 64 * 1024;

        // OpenGL buffer object
        buffer: gl.Buffer,
        // Mapped buffer memory
        map: []align(1) T,

        const Self = @This();

        pub fn init() Self {
            // Generate the buffer object
            const buffer = gl.Buffer.gen();
            // Bind it
            buffer.bind(.array_buffer);

            // Write-only persistent mapping.  Not coherent!
            const bs_flags = gl.BufferStorageFlags{ .map_write = true, .map_persistent = true };
            // Allocate buffer
            gl.bufferStorage(.array_buffer, T, VERTEX_BUFFER_LEN, null, bs_flags);

            // Remap the entire buffer
            const bm_flags = gl.BufferMapFlags{ .write = true, .persistent = true };
            const memory = gl.mapBufferRange(.array_buffer, T, 0, VERTEX_BUFFER_LEN, bm_flags);

            // Reset the buffer to 0 to avoid hard-to-reproduce bugs if we do something wrong with uninitialized memory
            for (memory) |*x| {
                x.* = std.mem.zeroes(T);
            }

            return .{
                .buffer = buffer,
                .map = memory,
            };
        }

        pub fn deinit(self: *Self) void {
            self.buffer.bind(.array_buffer);
            _ = gl.unmapBuffer(.array_buffer);
            self.buffer.delete();
        }

        pub fn set(self: *Self, index: u32, val: T) void {
            if (index >= VERTEX_BUFFER_LEN) {
                std.debug.panic("buffer overflow! i={}", .{index});
            }
            self.map[index] = val;
        }
    };
}

fn compile_shader(src: []const u8, shader_type: gl.ShaderType) gl.Shader {
    const shader = gl.createShader(shader_type);
    // Attempt to compile the shader
    shader.source(1, &[1][]const u8{src});
    shader.compile();

    // Extra bit of error checking in case we're not using a DEBUG OpenGL context and check_for_errors can't do it properly:
    const status = shader.get(.compile_status);
    if (status != @as(gl.Int, gl.binding.TRUE)) {
        std.debug.panic("Shader compilation failed!", .{});
    }

    return shader;
}

fn link_program(shaders: []const gl.Shader) gl.Program {
    const program = gl.createProgram();
    for (shaders) |shader| {
        program.attach(shader);
    }

    program.link();

    // Extra bit of error checking in case we're not using a DEBUG OpenGL context and check_for_errors can't do it properly:
    const status = program.get(.link_status);
    if (status != @as(gl.Int, gl.binding.TRUE)) {
        std.debug.panic("OpenGL program linking failed!", .{});
    }

    return program;
}

fn find_program_attrib(program: gl.Program, attr: [:0]const u8) gl.UInt {
    const index = gl.getAttribLocation(program, attr) orelse {
        std.debug.panic("Attribute \"{s}\" not found in program", .{attr});
    };

    return @as(gl.UInt, index);
}

const DebugSource = enum(gl.binding.GLenum) {
    other = gl.binding.DEBUG_SOURCE_OTHER,
    application = gl.binding.DEBUG_SOURCE_APPLICATION,
    third_party = gl.binding.DEBUG_SOURCE_THIRD_PARTY,
    shader_compiler = gl.binding.DEBUG_SOURCE_SHADER_COMPILER,
    window_system = gl.binding.DEBUG_SOURCE_WINDOW_SYSTEM,
    api = gl.binding.DEBUG_SOURCE_API,
};

const DebugSeverity = enum(gl.binding.GLenum) {
    notification = gl.binding.DEBUG_SEVERITY_NOTIFICATION,
    low = gl.binding.DEBUG_SEVERITY_LOW,
    medium = gl.binding.DEBUG_SEVERITY_MEDIUM,
    high = gl.binding.DEBUG_SEVERITY_HIGH,
};

const DebugType = enum(gl.binding.GLenum) {
    pop_group = gl.binding.DEBUG_TYPE_POP_GROUP,
    push_group = gl.binding.DEBUG_TYPE_PUSH_GROUP,
    marker = gl.binding.DEBUG_TYPE_MARKER,
    other = gl.binding.DEBUG_TYPE_OTHER,
    performance = gl.binding.DEBUG_TYPE_PERFORMANCE,
    portability = gl.binding.DEBUG_TYPE_PORTABILITY,
    undefined_behavior = gl.binding.DEBUG_TYPE_UNDEFINED_BEHAVIOR,
    deprecated_behavior = gl.binding.DEBUG_TYPE_DEPRECATED_BEHAVIOR,
    merror = gl.binding.DEBUG_TYPE_ERROR,
};

fn glCheckDebugMessageLog() void {
    var fatal = false;
    while (true) {
        var buffer: [4096]u8 = undefined;
        var _source: gl.binding.GLenum = undefined;
        var _mtype: gl.binding.GLenum = undefined;
        var id: gl.binding.GLuint = undefined;
        var _severity: gl.binding.GLenum = undefined;
        var length: gl.binding.GLsizei = undefined;
        const count = gl.binding.getDebugMessageLog(1, buffer.len, &_source, &_mtype, &id, &_severity, &length, &buffer);

        if (count == 0) {
            // No messages left
            break;
        }

        const source: DebugSource = @enumFromInt(_source);
        const severity: DebugSeverity = @enumFromInt(_severity);
        const mtype: DebugType = @enumFromInt(_mtype);

        std.debug.print("OpenGL [{}|{}|{}0x{x}] {s}", .{ severity, source, mtype, id, &buffer[0..@intCast(length)] });

        if (severity == .high) {
            // Something is very wrong, don't die just yet in order to display any additional error message
            fatal = true;
        }
    }

    if (fatal) {
        std.debug.panic("Fatal OpenGL error", .{});
    }
}

pub fn Renderer(comptime dbg: bool) type {
    return struct {
        window: SDL.Window,
        gl_context: SDL.gl.Context,

        // Vertex shared object
        vertex_shader: gl.Shader,
        // Fragment shader object
        fragment_shader: gl.Shader,
        // OpenGL Program object
        program: gl.Program,
        // OpenGl Vertex array object
        vertex_array_object: gl.VertexArray,
        // Buffer containing the vertice positions
        positions: Buffer(Position),
        // Buffer containing the vertice colors
        colors: Buffer(Color),
        // Current number of vertices in the buffers
        nvertices: u32,

        const Self = @This();

        fn glGetProcAddress(_: void, proc: [:0]const u8) ?*const anyopaque {
            return SDL.gl.getProcAddress(proc);
        }

        pub fn init() !Self {
            try SDL.init(.{
                .video = true,
                .events = true,
            });
            try SDL.gl.setAttribute(SDL.gl.Attribute{ .context_major_version = 4 });
            try SDL.gl.setAttribute(SDL.gl.Attribute{ .context_minor_version = 5 });
            if (dbg) {
                try SDL.gl.setAttribute(SDL.gl.Attribute{ .context_flags = .{ .debug = true } });
            }

            const window = try SDL.createWindow(
                "PSX",
                .centered,
                .centered,
                1024,
                512,
                .{ .vis = .shown, .context = .opengl },
            );

            const gl_context = try SDL.gl.createContext(window);
            try gl.loadExtensions({}, glGetProcAddress);

            gl.clearColor(0, 0, 0, 1);
            gl.clear(.{ .color = true });
            SDL.gl.swapWindow(window);

            const vs_src = @embedFile("shaders/vertex.glsl");
            const fs_src = @embedFile("shaders/fragment.glsl");

            // Compile our shaders...
            const vertex_shader = compile_shader(vs_src, .vertex);
            const fragment_shader = compile_shader(fs_src, .fragment);

            // Link our program
            const program = link_program(&.{ vertex_shader, fragment_shader });

            // And use it
            program.use();

            // Generate our vertex attribute object that will hold our vertex attributes
            const vao = gl.genVertexArray();
            // Bind our VAO
            vao.bind();

            // Setup the "position" attribute.  First we create the buffer holding the positions (this call also binds it)
            const positions = Buffer(Position).init();
            {
                // Then we retreive the index for the attribute in the shader
                const index = find_program_attrib(program, "vertex_position");
                // Enable it
                gl.enableVertexAttribArray(index);
                // Link the buffer and the index: 2 GLshort attributes, not normalized.  That should send the data untouched to the vertex shader.
                gl.vertexAttribIPointer(index, 2, .short, 0, 0);
            }

            const colors = Buffer(Color).init();
            {
                const index = find_program_attrib(program, "vertex_color");
                gl.enableVertexAttribArray(index);

                // Link the buffer and the index: 3 GLByte attributes, not normalized.  That should send the data untouched to the vertex shader.
                gl.vertexAttribIPointer(index, 3, .unsigned_byte, 0, 0);
            }

            if (dbg) {
                glCheckDebugMessageLog();
            }

            return .{
                .window = window,
                .gl_context = gl_context,
                .vertex_shader = vertex_shader,
                .fragment_shader = fragment_shader,
                .program = program,
                .vertex_array_object = vao,
                .positions = positions,
                .colors = colors,
                .nvertices = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.colors.deinit();
            self.positions.deinit();
            self.vertex_array_object.delete();
            self.vertex_shader.delete();
            self.fragment_shader.delete();
            self.program.delete();
            self.gl_context.delete();
            self.window.destroy();
            SDL.quit();
        }

        pub fn push_triangle(self: *Self, positions: [3]gpu_types.Xy, colors: [3]gpu_types.Color) void {

            // Make sure we have enough room left to queue the vertex
            if (self.nvertices + 3 > Buffer(void).VERTEX_BUFFER_LEN) {
                self.draw();
            }

            for (0..3) |i| {
                // Push
                self.positions.set(self.nvertices, Position.from_gpu_xy(positions[i]));
                self.colors.set(self.nvertices, Color.from_gpu_color(colors[i]));
                self.nvertices += 1;
            }
        }

        pub fn push_quad(self: *Self, positions: [4]gpu_types.Xy, colors: [4]gpu_types.Color) void {

            // Make sure we have enough room left to queue the vertex
            // We need to push two triangles to draw a quad, so 6 vertex
            if (self.nvertices + 6 > Buffer(void).VERTEX_BUFFER_LEN) {
                self.draw();
            }

            // Push the first triangle
            for (0..3) |i| {
                self.positions.set(self.nvertices, Position.from_gpu_xy(positions[i]));
                self.colors.set(self.nvertices, Color.from_gpu_color(colors[i]));
                self.nvertices += 1;
            }

            // Push the second triangle
            for (1..4) |i| {
                self.positions.set(self.nvertices, Position.from_gpu_xy(positions[i]));
                self.colors.set(self.nvertices, Color.from_gpu_color(colors[i]));
                self.nvertices += 1;
            }
        }

        // Draw the buffered commands and reset the buffer
        pub fn draw(self: *Self) void {
            // Make sure all the data from the persistent mappings is flushed to the buffer
            gl.binding.memoryBarrier(gl.binding.CLIENT_MAPPED_BUFFER_BARRIER_BIT);
            gl.drawArrays(.triangles, 0, self.nvertices);

            // Wait for GPU to complete
            const sync = gl.fenceSync();

            while (true) {
                const r = gl.clientWaitSync(sync, true, 10_000_000);
                if (r == .already_signaled or r == .condition_satisfied) {
                    // Drawing done
                    break;
                }
            }

            if (dbg) {
                glCheckDebugMessageLog();
            }

            // Reset the buffers
            self.nvertices = 0;
        }

        // Draw the buffered commands and display them
        pub fn display(self: *Self) void {
            // TODO: Make SDL window responsive
            _ = SDL.pollEvent();

            self.draw();
            SDL.gl.swapWindow(self.window);
        }
    };
}
