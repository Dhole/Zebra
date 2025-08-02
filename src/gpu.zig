const std = @import("std");

// GPU Registers

const Stat = packed struct {
    // Depth of the pixel values in a texture page
    const TextureDepth = enum(u2) {
        // 4 bits per pixel
        n_4bit = 0,
        // 8 bits per pixel
        n_8bit = 1,
        // 15 bits per pixel
        n_15bit = 2,
        // reserved
        reserved = 3,
    };

    // Requested DMA direction.
    const DmaDirection = enum(u2) {
        off = 0,
        fifo = 1,
        cpu_to_gp0 = 2,
        vram_to_cpu = 3,
    };

    // Video output vertical resolution
    const VerticalRes = enum(u1) {
        // 240 lines
        n_240 = 0,
        // 480 lines (only available for interlaced output)
        n_480 = 1,
    };

    const HorizontalRes = enum {
        n_256,
        n_320,
        n_512,
        n_640,
        n_386,
    };

    // Video Modes
    const VideoMode = enum(u1) {
        /// NTSC: 480i60H
        ntsc = 0,
        /// PAL: 576i50Hz
        pal = 1,
    };

    // Display area color depth
    const DisplayDepth = enum(u1) {
        /// 15 bits per pixel
        n_15bits = 0,
        /// 24 bits per pixel
        n_24bits = 1,
    };

    // Interlaced output splits each frame in two fields
    const Field = enum(u1) {
        // Top field (odd lines).
        top = 1,
        // Bottom field (even lines)
        bottom = 0,
    };

    // 1F801814h - GPUSTAT - GPU Status Register (R)
    // 0-3   Texture page X Base   (N*64)                              ;GP0(E1h).0-3
    // Texture page base X coordinate (4 bits, 64 byte increment)
    texture_page_x_base: u4 = 0,
    // 4     Texture page Y Base   (N*256) (ie. 0 or 256)              ;GP0(E1h).4
    // Texture page base Y coordinate (1bit, 256 line increment)
    texture_page_y_base: u1 = 0,
    // 5-6   Semi Transparency     (0=B/2+F/2, 1=B+F, 2=B-F, 3=B+F/4)  ;GP0(E1h).5-6
    // Semi-transparency. Not entirely sure how to handle that value
    // yet, it seems to describe how to blend the source and
    // destination colors.
    semi_transparency: u2 = 0,
    // 7-8   Texture page colors   (0=4bit, 1=8bit, 2=15bit, 3=Reserved)GP0(E1h).7-8
    // Texture page color depth
    texture_depth: TextureDepth = TextureDepth.n_4bit,
    // 9     Dither 24bit to 15bit (0=Off/strip LSBs, 1=Dither Enabled);GP0(E1h).9
    // Enable dithering from 24 to 15bits RGB
    dither: bool = false,
    // 10    Drawing to display area (0=Prohibited, 1=Allowed)         ;GP0(E1h).10
    // Allow drawing to the display area
    draw_to_display: bool = false,
    // 11    Set Mask-bit when drawing pixels (0=No, 1=Yes/Mask)       ;GP0(E6h).0
    // Force "mask" bit of the pixel to 1 when writing to VRAM
    // (otherwise don't modify it)
    force_set_mask_bit: bool = false,
    // 12    Draw Pixels           (0=Always, 1=Not to Masked areas)   ;GP0(E6h).1
    // Don't draw to pixels which have the "mask" bit set
    preserve_masked_pixels: bool = false,
    // 13    Interlace Field       (or, always 1 when GP1(08h).5=0)
    // Currently displayed field. For progressive output this is
    // always Top.
    field: Field = Field.top,
    // 14    "Reverseflag"         (0=Normal, 1=Distorted)             ;GP1(08h).7
    reverse_flag: u1 = 0,
    // 15    Texture Disable       (0=Normal, 1=Disable Textures)      ;GP0(E1h).11
    // When true all textures are disabled
    texture_disable: bool = false,
    // 16    Horizontal Resolution 2     (0=256/320/512/640, 1=368)    ;GP1(08h).6
    horizontal_res_2: u1 = 0,
    // 17-18 Horizontal Resolution 1     (0=256, 1=320, 2=512, 3=640)  ;GP1(08h).0-1
    horizontal_res_1: u2 = 0,
    // 19    Vertical Resolution         (0=240, 1=480, when Bit22=1)  ;GP1(08h).2
    vertical_res: VerticalRes = VerticalRes.n_240,
    // 20    Video Mode                  (0=NTSC/60Hz, 1=PAL/50Hz)     ;GP1(08h).3
    video_mode: VideoMode = VideoMode.ntsc,
    // 21    Display Area Color Depth    (0=15bit, 1=24bit)            ;GP1(08h).4
    // Display depth. The GPU itself always draws 15bit RGB, 24bit
    // output must use external assets (pre-rendered textures, MDEC,
    // etc...)
    display_depth: DisplayDepth = DisplayDepth.n_15bits,
    // 22    Vertical Interlace          (0=Off, 1=On)                 ;GP1(08h).5
    // Output interlaced video signal instead of progressive
    vertical_interlace: bool = false,
    // 23    Display Enable              (0=Enabled, 1=Disabled)       ;GP1(03h).0
    // Disable the display
    display_disabled: bool = true,
    // 24    Interrupt Request (IRQ1)    (0=Off, 1=IRQ)       ;GP0(1Fh)/GP1(02h)
    // True when the interrupt is active
    interrupt: bool = false,
    // 25    DMA / Data Request, meaning depends on GP1(04h) DMA Direction:
    //         When GP1(04h)=0 ---> Always zero (0)
    //         When GP1(04h)=1 ---> FIFO State  (0=Full, 1=Not Full)
    //         When GP1(04h)=2 ---> Same as GPUSTAT.28
    //         When GP1(04h)=3 ---> Same as GPUSTAT.27
    dma_data_req: u1 = 0,
    // 26    Ready to receive Cmd Word   (0=No, 1=Ready)  ;GP0(...) ;via GP0
    ready_recv_cmd: bool,
    // 27    Ready to send VRAM to CPU   (0=No, 1=Ready)  ;GP0(C0h) ;via GPUREAD
    ready_send_vram: bool,
    // 28    Ready to receive DMA Block  (0=No, 1=Ready)  ;GP0(...) ;via GP0
    ready_recv_dma: bool,
    // 29-30 DMA Direction (0=Off, 1=?, 2=CPUtoGP0, 3=GPUREADtoCPU)    ;GP1(04h).0-1
    // DMA request direction
    dma_direction: DmaDirection = DmaDirection.off,
    // 31    Drawing even/odd lines in interlace mode (0=Even or Vblank, 1=Odd)
    draw_odd_lines: bool = false,

    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }
    const Self = @This();

    fn init() Self {
        return Self{
            // For now we pretend that the GPU is always ready:
            // Ready to receive command
            .ready_recv_cmd = true,
            // Ready to send VRAM to CPU
            .ready_send_vram = true,
            // Ready to receive DMA block
            .ready_recv_dma = true,
        };
    }

    fn get(self: *Self) u32 {
        var v = self.*;
        const dma_data_req: u1 = switch (v.dma_direction) {
            DmaDirection.off => 0,
            DmaDirection.fifo => 1,
            DmaDirection.cpu_to_gp0 => @bitCast(v.ready_recv_dma),
            DmaDirection.vram_to_cpu => @bitCast(v.ready_send_vram),
        };
        v.dma_data_req = dma_data_req;
        return @bitCast(v);
    }

    // Video output horizontal resolution
    fn horizontal_res(self: *Self) HorizontalRes {
        if (self.horizontal_res_2 == 1) {
            return HorizontalRes.n_386;
        } else {
            return switch (self.horizontal_res_1) {
                0 => HorizontalRes.n_256,
                1 => HorizontalRes.n_320,
                2 => HorizontalRes.n_512,
                3 => HorizontalRes.n_640,
            };
        }
    }
};

const Gp0Packet = packed union {
    const Xy = packed struct {
        x: u16,
        y: u16,
        comptime {
            std.debug.assert(@bitSizeOf(@This()) == 32);
        }
    };

    xy: Xy,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }
};

const Gp0Cmd = packed struct {
    const Op = enum(u8) {
        // GP0(00h) - NOP (?)
        nop = 0x00,
        // GP0(01h) - Clear Cache
        clear_cache = 0x01,
        // GP0(28h) - Monochrome four-point polygon, opaque
        quad_mono_opaque = 0x28,
        // GP0(A0h) - Copy Rectangle (CPU to VRAM)
        image_load = 0xa0,
        // GP0(E1h) - Draw Mode setting (aka "Texpage")
        draw_mode = 0xe1,
        // GP0(E2h) - Texture Window setting
        texture_window = 0xe2,
        // GP0(E3h) - Set Drawing Area top left (X1,Y1)
        drawing_area_top_left = 0xe3,
        // GP0(E4h) - Set Drawing Area bottom right (X2,Y2)
        drawing_area_bottom_right = 0xe4,
        // GP0(E5h) - Set Drawing Offset (X,Y)
        drawing_offset = 0xe5,
        // GP0(E6h) - Mask Bit Setting
        bit_mask_setting = 0xe6,
        _,
    };

    const Args = packed union {
        const MonoPolygon = packed struct {
            red: u8,
            green: u8,
            blue: u8,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };
        const DrawMode = packed struct {
            // 0-3   Texture page X Base   (N*64) (ie. in 64-halfword steps)    ;GPUSTAT.0-3
            texture_page_x_base: u4,
            // 4     Texture page Y Base   (N*256) (ie. 0 or 256)               ;GPUSTAT.4
            texture_page_y_base: u1,
            // 5-6   Semi Transparency     (0=B/2+F/2, 1=B+F, 2=B-F, 3=B+F/4)   ;GPUSTAT.5-6
            semi_transparency: u2,
            // 7-8   Texture page colors   (0=4bit, 1=8bit, 2=15bit, 3=Reserved);GPUSTAT.7-8
            texture_depth: Stat.TextureDepth,
            // 9     Dither 24bit to 15bit (0=Off/strip LSBs, 1=Dither Enabled) ;GPUSTAT.9
            dither: bool,
            // 10    Drawing to display area (0=Prohibited, 1=Allowed)          ;GPUSTAT.10
            draw_to_display: bool,
            // 11    Texture Disable (0=Normal, 1=Disable if GP1(09h).Bit0=1)   ;GPUSTAT.15
            //         (Above might be chipselect for (absent) second VRAM chip?)
            texture_disable: bool,
            // 12    Textured Rectangle X-Flip   (BIOS does set this bit on power-up...?)
            texture_rectangle_x_flip: bool,
            // 13    Textured Rectangle Y-Flip   (BIOS does set it equal to GPUSTAT.13...?)
            texture_rectangle_y_flip: bool,
            // 14-23 Not used (should be 0)
            unused: u10,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const DrawingArea = packed struct {
            // 0-9    X-coordinate (0..1023)
            x_coord: u10,
            // 10-18  Y-coordinate (0..511)   ;\on Old 160pin GPU (max 1MB VRAM)
            // 10-19  Y-coordinate (0..1023)  ;\on New 208pin GPU (max 2MB VRAM)
            y_coord: u10,
            // 20-23  Not used (zero)         ;/(retail consoles have only 1MB though)
            unused_1: u4,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const DrawingOffset = packed struct {
            // 0-10   X-offset (-1024..+1023) (usually within X1,X2 of Drawing Area)
            x_offset: i11,
            // 11-21  Y-offset (-1024..+1023) (usually within Y1,Y2 of Drawing Area)
            y_offset: i11,
            // 22-23  Not used (zero)
            unused: u2,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const TextureWindow = packed struct {
            // 0-4    Texture window Mask X   (in 8 pixel steps)
            texture_window_x_mask: u5,
            // 5-9    Texture window Mask Y   (in 8 pixel steps)
            texture_window_y_mask: u5,
            // 10-14  Texture window Offset X (in 8 pixel steps)
            texture_window_x_offset: u5,
            // 15-19  Texture window Offset Y (in 8 pixel steps)
            texture_window_y_offset: u5,
            // 20-23  Not used (zero)
            unused: u4,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const BitMaskSetting = packed struct {
            // 0     Set mask while drawing (0=TextureBit15, 1=ForceBit15=1)   ;GPUSTAT.11
            force_set_mask_bit: bool,
            // 1     Check mask before draw (0=Draw Always, 1=Draw if Bit15=0) ;GPUSTAT.12
            preserve_masked_pixels: bool,
            // 2-23  Not used (zero)
            unused: u22,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        mono_polygon: MonoPolygon,
        draw_mode: DrawMode,
        drawing_area: DrawingArea,
        drawing_offset: DrawingOffset,
        texture_window: TextureWindow,
        bit_mask_setting: BitMaskSetting,
        raw: u24,
        comptime {
            std.debug.assert(@bitSizeOf(@This()) == 24);
        }
    };

    args: Args,
    op: Op,
};

const Gp1Cmd = packed struct {
    const Op = enum(u8) {
        // GP1(00h) - Reset GPU
        reset = 0x00,
        // GP1(03h) - Display Enable
        display_enable = 0x03,
        // GP1(04h) - DMA Direction / Data Request
        dma_direction = 0x04,
        // GP1(05h) - Start of Display area (in VRAM)
        display_vram_start = 0x05,
        // GP1(06h) - Horizontal Display range (on Screen)
        display_horizontal_range = 0x06,
        // GP1(07h) - Vertical Display range (on Screen)
        display_vertical_range = 0x07,
        // GP1(08h) - Display mode
        display_mode = 0x08,
        _,
    };

    const Args = packed union {
        const DisplayMode = packed struct {
            // 0-1   Horizontal Resolution 1     (0=256, 1=320, 2=512, 3=640) ;GPUSTAT.17-18
            horizontal_res_1: u2,
            // 2     Vertical Resolution         (0=240, 1=480, when Bit5=1)  ;GPUSTAT.19
            vertical_res: Stat.VerticalRes,
            // 3     Video Mode                  (0=NTSC/60Hz, 1=PAL/50Hz)    ;GPUSTAT.20
            video_mode: Stat.VideoMode,
            // 4     Display Area Color Depth    (0=15bit, 1=24bit)           ;GPUSTAT.21
            display_depth: Stat.DisplayDepth,
            // 5     Vertical Interlace          (0=Off, 1=On)                ;GPUSTAT.22
            vertical_interlace: bool,
            // 6     Horizontal Resolution 2     (0=256/320/512/640, 1=368)   ;GPUSTAT.16
            horizontal_res_2: u1,
            // 7     "Reverseflag"               (0=Normal, 1=Distorted)      ;GPUSTAT.14
            reverse_flag: u1,
            // 8-23  Not used (zero)
            unused: u16,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const DirectionArgs = packed struct {
            // 0-1  DMA Direction (0=Off, 1=FIFO, 2=CPUtoGP0, 3=GPUREADtoCPU) ;GPUSTAT.29-30
            dma_direction: Stat.DmaDirection,
            // 2-23 Not used (zero)
            unused: u22,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const DisplayVramStart = packed struct {
            // 0-9   X (0-1023)    (halfword address in VRAM)  (relative to begin of VRAM)
            x: u10,
            // 10-18 Y (0-511)     (scanline number in VRAM)   (relative to begin of VRAM)
            y: u9,
            // 19-23 Not used (zero)
            unused: u5,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const DisplayHorizontalRange = packed struct {
            // 0-11   X1 (260h+0)       ;12bit       ;\counted in 53.222400MHz units,
            x1: u12,
            // 12-23  X2 (260h+320*8)   ;12bit       ;/relative to HSYNC
            x2: u12,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const DisplayVerticalRange = packed struct {
            // 0-9   Y1 (NTSC=88h-(224/2), (PAL=A3h-(264/2))  ;\scanline numbers on screen,
            y1: u10,
            // 10-19 Y2 (NTSC=88h+(224/2), (PAL=A3h+(264/2))  ;/relative to VSYNC
            y2: u10,
            // 20-23 Not used (zero)
            unused: u4,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        const DisplayEnable = packed struct {
            // 0     Display On/Off   (0=On, 1=Off)                         ;GPUSTAT.23
            off: bool,
            // 1-23  Not used (zero)
            unused: u23,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        display_enable: DisplayEnable,
        display_mode: DisplayMode,
        dma_direction: DirectionArgs,
        display_vram_start: DisplayVramStart,
        display_horizontal_range: DisplayHorizontalRange,
        display_vertical_range: DisplayVerticalRange,
        raw: u24,
        comptime {
            std.debug.assert(@bitSizeOf(@This()) == 24);
        }
    };

    args: Args,
    op: Op,
};

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

pub const Gpu = struct {
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

    const Self = @This();

    pub fn init() Self {
        return Self{
            .stat = Stat.init(),
            .gp0_cmd_buffer = CmdBuffer.init(),
            .gp0_words_rem = 0,
            .gp0_cmd_fn = Self.gp0_dummy,
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
                Gp0Cmd.Op.image_load => {
                    self.gp0_cmd_buffer.push_word(v);
                    self.gp0_words_rem = 2;
                    self.gp0_cmd_fn = Self.image_load_0;
                },
                Gp0Cmd.Op.draw_mode => self.draw_mode(cmd.args.draw_mode),
                Gp0Cmd.Op.texture_window => self.texture_window(cmd.args.texture_window),
                Gp0Cmd.Op.drawing_area_top_left => self.drawing_area_top_left(cmd.args.drawing_area),
                Gp0Cmd.Op.drawing_area_bottom_right => self.drawing_area_bottom_right(cmd.args.drawing_area),
                Gp0Cmd.Op.drawing_offset => self.drawing_offset(cmd.args.drawing_offset),
                Gp0Cmd.Op.bit_mask_setting => self.bit_mask_setting(cmd.args.bit_mask_setting),
                _ => {
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
        const color = @as(Gp0Cmd, @bitCast(buffer[0])).args.mono_polygon;
        const vertex1 = @as(Gp0Packet, @bitCast(buffer[1])).xy;
        const vertex2 = @as(Gp0Packet, @bitCast(buffer[2])).xy;
        const vertex3 = @as(Gp0Packet, @bitCast(buffer[3])).xy;
        const vertex4 = @as(Gp0Packet, @bitCast(buffer[4])).xy;

        _ = self;
        std.debug.print("TODO: Draw quad {} [{}, {}, {}, {}]\n", .{ color, vertex1, vertex2, vertex3, vertex4 });
    }

    // GP0(A0): Copy Rectangle (CPU to VRAM)
    fn image_load_0(self: *Self, buffer: *[CmdBuffer.SIZE]u32) void {
        const dst_coord = @as(Gp0Packet, @bitCast(buffer[1])).xy;
        // TODO: store this dst_coord
        _ = dst_coord;
        const size = @as(Gp0Packet, @bitCast(buffer[2])).xy;
        // Each pixel is 16 bits
        const num_pixels: usize = size.x * size.y;
        // Transfer 32 bits at a time.  If the number of pixels is odd we'll
        // have an extra 16 bit of padding
        const num_words = divCeil(num_pixels, 2);
        self.gp0_mode = Gp0Mode.image_load;
        self.gp0_words_rem = num_words;
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
    }

    // GP0(E6): Mask Bit Setting
    fn bit_mask_setting(self: *Self, args: Gp0Cmd.Args.BitMaskSetting) void {
        self.stat.force_set_mask_bit = args.force_set_mask_bit;
        self.stat.preserve_masked_pixels = args.preserve_masked_pixels;
    }

    fn gp1(self: *Self, v: u32) void {
        const cmd: Gp1Cmd = @bitCast(v);
        switch (cmd.op) {
            Gp1Cmd.Op.reset => self.reset(),
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

        // TODO should also clear the command FIFO when we implement it
        // TODO should also invalidate GPU cache if we ever implement it
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
