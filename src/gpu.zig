const std = @import("std");

const gpu_types = @import("gpu_types.zig");
const Stat = gpu_types.Stat;
const Gp0Packet = gpu_types.Gp0Packet;
const Xy = gpu_types.Xy;
const Color = gpu_types.Color;
const Gp0Cmd = gpu_types.Gp0Cmd;
const Gp1Cmd = gpu_types.Gp1Cmd;

const CmdBuffer = struct {
    // Command buffer: the longuest possible command is GP0(0x3E)
    // which takes 12 parameters
    const SIZE = 12;
    buffer: [SIZE]u32,
    len: u8,

    const Self = @This();

    fn init() Self {
        return Self{
            .buffer = .{0} ** 12,
            .len = 0,
        };
    }

    fn clear(self: *Self) void {
        self.len = 0;
    }

    fn push_word(self: *Self, word: u32) void {
        self.buffer[self.len] = word;
        self.len += 1;
    }
};

fn divCeil(numerator: usize, denominator: usize) usize {
    return @divFloor(numerator - 1, denominator) + 1;
}

pub fn Gpu(comptime Renderer: type) type {
    return struct {
        pub const ADDR_START: u16 = 0x0810;
        pub const ADDR_END: u16 = 0x0817;

        const RegRead = enum(u16) {
            read = 0x0810, // 1F801810.Read  4 GPUREAD Read responses to GP0(C0h) and GP1(10h) commands
            stat = 0x0814, // 1F801814.Read  4 GPUSTAT Read GPU Status Register
            _,
        };
        const RegWrite = enum(u16) {
            gp0 = 0x0810, //  1F801810.Write 4 GP0 Send GP0 Commands/Packets (Rendering and VRAM Access)
            gp1 = 0x0814, //  1F801814.Write 4 GP1 Send GP1 Commands (Display Control)
            _,
        };
        const Gp0Mode = enum {
            // Default mode: handling commands
            command,
            // Loading an image into VRAM
            image_load,
        };

        stat: Stat,

        // Mirror textured rectangles along the x axis
        texture_rectangle_x_flip: bool = false,
        // Mirror textured rectangles along the y axis
        texture_rectangle_y_flip: bool = false,
        // Texture window x mask (8 pixel steps)
        texture_window_x_mask: u8 = 0,
        // Texture window y mask (8 pixel steps)
        texture_window_y_mask: u8 = 0,
        // Texture window x offset (8 pixel steps)
        texture_window_x_offset: u8 = 0,
        // Texture window y offset (8 pixel steps)
        texture_window_y_offset: u8 = 0,
        // Left-most column of drawing area
        drawing_area_left: u16 = 0,
        // Top-most line of drawing area
        drawing_area_top: u16 = 0,
        // Right-most column of drawing area
        drawing_area_right: u16 = 0,
        // Bottom-most line of drawing area
        drawing_area_bottom: u16 = 0,
        // Horizontal drawing offset applied to all vertex
        drawing_x_offset: i16 = 0,
        // Vertical drawing offset applied to all vertex
        drawing_y_offset: i16 = 0,
        // First column of the display area in VRAM
        display_vram_x_start: u16 = 0,
        // First line of the display area in VRAM
        display_vram_y_start: u16 = 0,
        // Display output horizontal start relative to HSYNC
        display_horiz_start: u16 = 0x200,
        // Display output horizontal end relative to HSYNC
        display_horiz_end: u16 = 0xc00,
        // Display output first line relative to VSYNC
        display_line_start: u16 = 0x10,
        // Display output last line relative to VSYNC
        display_line_end: u16 = 0x100,

        gp0_mode: Gp0Mode = Gp0Mode.command,
        gp0_cmd_buffer: CmdBuffer,
        gp0_words_rem: usize,
        gp0_cmd_fn: *const fn (*Self, buffer: *[CmdBuffer.SIZE]u32) void,

        renderer: Renderer,

        const Self = @This();

        pub fn init(renderer: Renderer) Self {
            return Self{
                .stat = Stat.init(),
                .gp0_cmd_buffer = CmdBuffer.init(),
                .gp0_words_rem = 0,
                .gp0_cmd_fn = Self.gp0_dummy,
                .renderer = renderer,
            };
        }

        fn gp0_dummy(self: *Self, buffer: *[12]u32) void {
            _ = self;
            _ = buffer;
        }

        pub fn read_u32(self: *Self, addr: u16) u32 {
            const reg: RegRead = @enumFromInt(addr);
            switch (reg) {
                RegRead.read => {
                    // TODO
                    return 0;
                },
                RegRead.stat => {
                    return self.stat.get();
                },
                _ => unreachable,
            }
        }
        pub fn write_u32(self: *Self, addr: u16, v: u32) void {
            const reg: RegWrite = @enumFromInt(addr);
            switch (reg) {
                RegWrite.gp0 => {
                    self.gp0(v);
                },
                RegWrite.gp1 => {
                    self.gp1(v);
                },
                _ => unreachable,
            }
        }

        pub fn gp0(self: *Self, v: u32) void {
            std.debug.print("DBG gp0 {} {x:0>8}\n", .{ self.gp0_mode, v });
            if (self.gp0_words_rem == 0) {
                const cmd: Gp0Cmd = @bitCast(v);
                switch (cmd.op) {
                    Gp0Cmd.Op.nop => {},
                    Gp0Cmd.Op.clear_cache => self.clear_cache(),
                    Gp0Cmd.Op.quad_mono_opaque => {
                        self.gp0_cmd_buffer.push_word(v);
                        self.gp0_words_rem = 4;
                        self.gp0_cmd_fn = Self.quad_mono_opaque;
                    },
                    Gp0Cmd.Op.quad_texture_blend_opaque => {
                        self.gp0_cmd_buffer.push_word(v);
                        self.gp0_words_rem = 8;
                        self.gp0_cmd_fn = Self.quad_texture_blend_opaque;
                    },
                    Gp0Cmd.Op.triangle_shaded_opaque => {
                        self.gp0_cmd_buffer.push_word(v);
                        self.gp0_words_rem = 5;
                        self.gp0_cmd_fn = Self.triangle_shaded_opaque;
                    },
                    Gp0Cmd.Op.quad_shaded_opaque => {
                        self.gp0_cmd_buffer.push_word(v);
                        self.gp0_words_rem = 7;
                        self.gp0_cmd_fn = Self.quad_shaded_opaque;
                    },
                    Gp0Cmd.Op.image_load => {
                        self.gp0_cmd_buffer.push_word(v);
                        self.gp0_words_rem = 2;
                        self.gp0_cmd_fn = Self.image_load_0;
                    },
                    Gp0Cmd.Op.image_store => {
                        self.gp0_cmd_buffer.push_word(v);
                        self.gp0_words_rem = 2;
                        self.gp0_cmd_fn = Self.image_store;
                    },
                    Gp0Cmd.Op.draw_mode => self.draw_mode(cmd.args.draw_mode),
                    Gp0Cmd.Op.texture_window => self.texture_window(cmd.args.texture_window),
                    Gp0Cmd.Op.drawing_area_top_left => self.drawing_area_top_left(cmd.args.drawing_area),
                    Gp0Cmd.Op.drawing_area_bottom_right => self.drawing_area_bottom_right(cmd.args.drawing_area),
                    Gp0Cmd.Op.drawing_offset => self.drawing_offset(cmd.args.drawing_offset),
                    Gp0Cmd.Op.bit_mask_setting => self.bit_mask_setting(cmd.args.bit_mask_setting),
                    _ => {
                        // const op: u8 = @intFromEnum(cmd.op);
                        // switch (op) {
                        //     0x31, 0x45, 0x56 => {
                        //         std.debug.panic("BAD gp0 {x:0>2}\n", .{op});
                        //         return;
                        //     },
                        //     else => {},
                        // }
                        // std.debug.print("TODO gp0 {x:0>2}\n", .{op});
                        std.debug.print("GPU: \n{}\n", .{self});
                        std.debug.panic("TODO: gpu gp0 op {x:0>2} args {x:0>6}", .{ cmd.op, cmd.args.raw });
                    },
                }
            } else {
                switch (self.gp0_mode) {
                    Gp0Mode.command => {
                        self.gp0_cmd_buffer.push_word(v);
                        self.gp0_words_rem -= 1;
                        if (self.gp0_words_rem == 0) {
                            self.gp0_cmd_fn(self, &self.gp0_cmd_buffer.buffer);
                            self.gp0_cmd_buffer.clear();
                        }
                    },
                    Gp0Mode.image_load => {
                        // TODO
                        self.gp0_words_rem -= 1;
                        if (self.gp0_words_rem == 0) {
                            // Load done, switch back to command mode
                            self.gp0_mode = Gp0Mode.command;
                        }
                    },
                }
            }
        }

        // GP0(01): Clear Cache
        fn clear_cache(self: *Self) void {
            _ = self;
            // TODO
        }

        // GP0(28): Monochrome four-point polygon, opaque
        fn quad_mono_opaque(self: *Self, buffer: *[CmdBuffer.SIZE]u32) void {
            const color = @as(Gp0Cmd, @bitCast(buffer[0])).args.color;
            const vertex1 = @as(Gp0Packet, @bitCast(buffer[1])).xy;
            const vertex2 = @as(Gp0Packet, @bitCast(buffer[2])).xy;
            const vertex3 = @as(Gp0Packet, @bitCast(buffer[3])).xy;
            const vertex4 = @as(Gp0Packet, @bitCast(buffer[4])).xy;

            self.renderer.push_quad(
                [_]Xy{ vertex1, vertex2, vertex3, vertex4 },
                [_]Color{ color, color, color, color },
            );
        }

        // GP0(2C): Textured four-point polygon, opaque, texture-blending
        fn quad_texture_blend_opaque(self: *Self, buffer: *[CmdBuffer.SIZE]u32) void {
            const color1 = @as(Gp0Cmd, @bitCast(buffer[0])).args.color;
            const vertex1 = @as(Gp0Packet, @bitCast(buffer[1])).xy;
            const texcoord1, const palette = blk: {
                const lo_hi = @as(Gp0Packet, @bitCast(buffer[2])).lo_hi;
                break :blk .{ lo_hi.lo.xy, lo_hi.hi.raw };
            };
            const vertex2 = @as(Gp0Packet, @bitCast(buffer[3])).xy;
            const texcoord2, const texpage = blk: {
                const lo_hi = @as(Gp0Packet, @bitCast(buffer[4])).lo_hi;
                break :blk .{ lo_hi.lo.xy, lo_hi.hi.raw };
            };
            const vertex3 = @as(Gp0Packet, @bitCast(buffer[5])).xy;
            const texcoord3 = @as(Gp0Packet, @bitCast(buffer[6])).lo_hi.lo.xy;
            const vertex4 = @as(Gp0Packet, @bitCast(buffer[7])).xy;
            const texcoord4 = @as(Gp0Packet, @bitCast(buffer[8])).lo_hi.lo.xy;

            std.debug.print("TODO: Draw shaded quad tex {} [{}, {}, {}, {}] [{}, {}, {}, {}], {}, {}\n", .{ color1, vertex1, vertex2, vertex3, vertex4, texcoord1, texcoord2, texcoord3, texcoord4, palette, texpage });

            // TODO: We don't support textures for now, use a solid red color insetead
            const color = Color{ .red = 0x80, .green = 0x00, .blue = 0x00 };
            self.renderer.push_quad(
                [_]Xy{ vertex1, vertex2, vertex3, vertex4 },
                [_]Color{ color, color, color, color },
            );
        }

        // GP0(30): Shaded three-point polygon, opaque
        fn triangle_shaded_opaque(self: *Self, buffer: *[CmdBuffer.SIZE]u32) void {
            const color1 = @as(Gp0Cmd, @bitCast(buffer[0])).args.color;
            const vertex1 = @as(Gp0Packet, @bitCast(buffer[1])).xy;
            const color2 = @as(Gp0Packet, @bitCast(buffer[2])).color.v;
            const vertex2 = @as(Gp0Packet, @bitCast(buffer[3])).xy;
            const color3 = @as(Gp0Packet, @bitCast(buffer[4])).color.v;
            const vertex3 = @as(Gp0Packet, @bitCast(buffer[5])).xy;

            self.renderer.push_triangle(
                [_]Xy{ vertex1, vertex2, vertex3 },
                [_]Color{ color1, color2, color3 },
            );
        }

        // GP0(38): Shaded four-point polygon, opaque
        fn quad_shaded_opaque(self: *Self, buffer: *[CmdBuffer.SIZE]u32) void {
            const color1 = @as(Gp0Cmd, @bitCast(buffer[0])).args.color;
            const vertex1 = @as(Gp0Packet, @bitCast(buffer[1])).xy;
            const color2 = @as(Gp0Packet, @bitCast(buffer[2])).color.v;
            const vertex2 = @as(Gp0Packet, @bitCast(buffer[3])).xy;
            const color3 = @as(Gp0Packet, @bitCast(buffer[4])).color.v;
            const vertex3 = @as(Gp0Packet, @bitCast(buffer[5])).xy;
            const color4 = @as(Gp0Packet, @bitCast(buffer[6])).color.v;
            const vertex4 = @as(Gp0Packet, @bitCast(buffer[7])).xy;

            self.renderer.push_quad(
                [_]Xy{ vertex1, vertex2, vertex3, vertex4 },
                [_]Color{ color1, color2, color3, color4 },
            );
        }

        // GP0(A0): Copy Rectangle (CPU to VRAM)
        fn image_load_0(self: *Self, buffer: *[CmdBuffer.SIZE]u32) void {
            const dst_coord = @as(Gp0Packet, @bitCast(buffer[1])).xy;
            // TODO: store this dst_coord
            // _ = dst_coord;
            const size = @as(Gp0Packet, @bitCast(buffer[2])).xy;
            // Each pixel is 16 bits
            const num_pixels: usize = @as(usize, size.x) * @as(usize, size.y);
            // Transfer 32 bits at a time.  If the number of pixels is odd we'll
            // have an extra 16 bit of padding
            const num_words = divCeil(num_pixels, 2);
            std.debug.print("TODO image_load_1 dst_coord={}, size={}, num_words={}\n", .{ dst_coord, size, num_words });
            self.gp0_mode = Gp0Mode.image_load;
            self.gp0_words_rem = num_words;
        }

        // GP0(C0h) - Copy Rectangle (VRAM to CPU)
        fn image_store(self: *Self, buffer: *[CmdBuffer.SIZE]u32) void {
            _ = self;
            const src_coord = @as(Gp0Packet, @bitCast(buffer[1])).xy;
            // TODO: store this src_coord
            const size = @as(Gp0Packet, @bitCast(buffer[2])).xy;
            // Each pixel is 16 bits
            const num_pixels: usize = @as(usize, size.x) * @as(usize, size.y);
            // Transfer 32 bits at a time.  If the number of pixels is odd we'll
            // have an extra 16 bit of padding
            const num_words = divCeil(num_pixels, 2);
            std.debug.print("TODO image_store src_coord={}, size={}, num_words={}\n", .{ src_coord, size, num_words });
        }

        // GP0(e1)
        fn draw_mode(self: *Self, args: Gp0Cmd.Args.DrawMode) void {
            self.stat.texture_page_x_base = args.texture_page_x_base;
            self.stat.texture_page_y_base = args.texture_page_y_base;
            self.stat.semi_transparency = args.semi_transparency;
            if (args.texture_depth == Stat.TextureDepth.reserved) {
                std.debug.panic("texture_depth=3 is reserved", .{});
            }
            self.stat.texture_depth = args.texture_depth;
            self.stat.dither = args.dither;
            self.stat.draw_to_display = args.draw_to_display;
            self.stat.texture_disable = args.texture_disable;
            self.texture_rectangle_x_flip = args.texture_rectangle_x_flip;
            self.texture_rectangle_y_flip = args.texture_rectangle_y_flip;
        }

        // GP0(E2): Texture Window setting
        fn texture_window(self: *Self, args: Gp0Cmd.Args.TextureWindow) void {
            self.texture_window_x_mask = args.texture_window_x_mask;
            self.texture_window_y_mask = args.texture_window_y_mask;
            self.texture_window_x_offset = args.texture_window_x_offset;
            self.texture_window_y_offset = args.texture_window_y_offset;
        }

        // GP0(E3): Set Drawing Area top left (X1,Y1)
        fn drawing_area_top_left(self: *Self, args: Gp0Cmd.Args.DrawingArea) void {
            self.drawing_area_left = args.x_coord;
            self.drawing_area_top = args.y_coord;
        }

        // GP0(E4): Set Drawing Area bottom right (X2,Y2)
        fn drawing_area_bottom_right(self: *Self, args: Gp0Cmd.Args.DrawingArea) void {
            self.drawing_area_right = args.x_coord;
            self.drawing_area_bottom = args.y_coord;
        }

        // GP0(E5): Set Drawing Offset (X,Y)
        fn drawing_offset(self: *Self, args: Gp0Cmd.Args.DrawingOffset) void {
            self.drawing_x_offset = args.x_offset;
            self.drawing_y_offset = args.y_offset;

            // TODO: Temporary hack: force display when changing offset since we don't have proper timings
            self.renderer.display();
        }

        // GP0(E6): Mask Bit Setting
        fn bit_mask_setting(self: *Self, args: Gp0Cmd.Args.BitMaskSetting) void {
            self.stat.force_set_mask_bit = args.force_set_mask_bit;
            self.stat.preserve_masked_pixels = args.preserve_masked_pixels;
        }

        fn gp1(self: *Self, v: u32) void {
            std.debug.print("DBG gp1 {x:0>8}\n", .{v});
            const cmd: Gp1Cmd = @bitCast(v);
            switch (cmd.op) {
                Gp1Cmd.Op.reset => self.reset(),
                Gp1Cmd.Op.reset_command_buffer => self.reset_command_buffer(),
                Gp1Cmd.Op.acknowledge_irq => self.acknowledge_irq(),
                Gp1Cmd.Op.display_enable => self.display_enable(cmd.args.display_enable),
                Gp1Cmd.Op.dma_direction => self.dma_direction(cmd.args.dma_direction),
                Gp1Cmd.Op.display_vram_start => self.display_vram_start(cmd.args.display_vram_start),
                Gp1Cmd.Op.display_horizontal_range => self.display_horizontal_range(cmd.args.display_horizontal_range),
                Gp1Cmd.Op.display_vertical_range => self.display_vertical_range(cmd.args.display_vertical_range),
                Gp1Cmd.Op.display_mode => self.display_mode(cmd.args.display_mode),
                _ => std.debug.panic("TODO: gpu gp1 op {x:0>2} args {x:0>6}", .{ cmd.op, cmd.args.raw }),
            }
        }

        // GP1(00): soft reset
        fn reset(self: *Self) void {
            self.stat.interrupt = false;

            self.stat.texture_page_x_base = 0;
            self.stat.texture_page_y_base = 0;
            self.stat.semi_transparency = 0;
            self.stat.texture_depth = Stat.TextureDepth.n_4bit;
            self.texture_window_x_mask = 0;
            self.texture_window_y_mask = 0;
            self.texture_window_x_offset = 0;
            self.texture_window_y_offset = 0;
            self.stat.dither = false;
            self.stat.draw_to_display = false;
            self.stat.texture_disable = false;
            self.texture_rectangle_x_flip = false;
            self.texture_rectangle_y_flip = false;
            self.drawing_area_left = 0;
            self.drawing_area_top = 0;
            self.drawing_area_right = 0;
            self.drawing_area_bottom = 0;
            self.drawing_x_offset = 0;
            self.drawing_y_offset = 0;
            self.stat.force_set_mask_bit = false;
            self.stat.preserve_masked_pixels = false;

            self.stat.dma_direction = Stat.DmaDirection.off;

            self.stat.display_disabled = true;
            self.display_vram_x_start = 0;
            self.display_vram_y_start = 0;
            self.stat.horizontal_res_1 = 0;
            self.stat.horizontal_res_2 = 0;
            self.stat.vertical_res = Stat.VerticalRes.n_240;

            self.stat.video_mode = Stat.VideoMode.ntsc;
            self.stat.vertical_interlace = true;
            self.display_horiz_start = 0x200;
            self.display_horiz_end = 0xc00;
            self.display_line_start = 0x10;
            self.display_line_end = 0x100;
            self.stat.display_depth = Stat.DisplayDepth.n_15bits;

            self.reset_command_buffer();
            // TODO should also invalidate GPU cache if we ever implement it
        }

        // GP1(01): Reset Command Buffer
        fn reset_command_buffer(self: *Self) void {
            self.gp0_cmd_buffer.clear();
            self.gp0_words_rem = 0;
            self.gp0_mode = Gp0Mode.command;
            // TODO: Should also clear the command FIFO when we implement it
        }

        // GP1(02): Acknowledge GPU Interrupt (IRQ1)
        fn acknowledge_irq(self: *Self) void {
            self.stat.interrupt = false;
        }

        // GP1(03): Display Enable
        fn display_enable(self: *Self, args: Gp1Cmd.Args.DisplayEnable) void {
            self.stat.display_disabled = args.off;
        }

        // GP1(04): DMA Direction / Data Request
        fn dma_direction(self: *Self, args: Gp1Cmd.Args.DirectionArgs) void {
            self.stat.dma_direction = args.dma_direction;
        }

        // GP1(05h): Start of Display area (in VRAM)
        fn display_vram_start(self: *Self, args: Gp1Cmd.Args.DisplayVramStart) void {
            const mask_inv: u10 = 0b1;
            self.display_vram_x_start = args.x & (~mask_inv);
            self.display_vram_y_start = args.y;
        }

        // GP1(06): Horizontal Display range (on Screen)
        fn display_horizontal_range(self: *Self, args: Gp1Cmd.Args.DisplayHorizontalRange) void {
            self.display_horiz_start = args.x1;
            self.display_horiz_end = args.x2;
        }

        // GP1(07): Vertical Display range (on Screen)
        fn display_vertical_range(self: *Self, args: Gp1Cmd.Args.DisplayVerticalRange) void {
            self.display_line_start = args.y1;
            self.display_line_end = args.y2;
        }

        // GP1(08): Display mode
        fn display_mode(self: *Self, args: Gp1Cmd.Args.DisplayMode) void {
            self.stat.horizontal_res_1 = args.horizontal_res_1;
            self.stat.horizontal_res_2 = args.horizontal_res_2;
            self.stat.video_mode = args.video_mode;
            self.stat.display_depth = args.display_depth;
            self.stat.vertical_interlace = args.vertical_interlace;
            self.stat.horizontal_res_2 = args.horizontal_res_2;
            if (args.reverse_flag == 1) {
                std.debug.panic("unsupported: reverse_flag=1", .{});
            }
            self.stat.reverse_flag = args.reverse_flag;
        }
    };
}
