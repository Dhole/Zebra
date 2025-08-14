const std = @import("std");
const SDL = @import("SDL2");
const gl = @import("zgl");

pub const Renderer = struct {
    window: SDL.Window,
    gl_context: SDL.gl.Context,

    const Self = @This();

    fn glGetProcAddress(_: void, proc: [:0]const u8) ?*const anyopaque {
        return SDL.gl.getProcAddress(proc);
    }

    pub fn init() !Self {
        std.debug.print("hello 1\n", .{});

        try SDL.init(.{
            .video = true,
        });
        try SDL.gl.setAttribute(SDL.gl.Attribute{ .context_major_version = 4 });
        try SDL.gl.setAttribute(SDL.gl.Attribute{ .context_minor_version = 5 });

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

        return .{
            .window = window,
            .gl_context = gl_context,
        };
    }

    pub fn deinit(self: *Self) void {
        self.gl_context.delete();
        self.window.destroy();
        SDL.quit();
    }
};
