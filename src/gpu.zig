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
    set_mask_bits: bool = false,
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

const ArgsDrawMode = packed struct {
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

const Gp0CommandArg = packed union {
    draw_mode: ArgsDrawMode,
};

const Gp0Op = enum(u8) {
    // GP0(00h) - NOP (?)
    nop = 0x00,
    // GP0(E1h) - Draw Mode setting (aka "Texpage")
    draw_mode = 0xe1,
    _,
};

const Gp0Command = packed struct {
    args: Gp0CommandArg,
    op: Gp0Op,
};

const Gp1Op = enum(u8) {
    // GP1(00h) - Reset GPU
    reset = 0x00,
    _,
};

const Gp1Command = packed struct {
    args: u24,
    op: Gp1Op,
};

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

    const Self = @This();

    pub fn init() Self {
        return Self{
            .stat = Stat.init(),
        };
    }

    pub fn read_u32(self: *Self, addr: u16) u32 {
        const reg: RegRead = @enumFromInt(addr);
        switch (reg) {
            RegRead.read => {
                std.debug.print("TODO: gpu.read_u32 READ\n", .{});
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
                std.debug.print("TODO: gpu.write_u32 GP1 value {x:0>8}\n", .{v});
            },
            _ => unreachable,
        }
    }

    pub fn gp0(self: *Self, v: u32) void {
        const cmd: Gp0Command = @bitCast(v);
        switch (cmd.op) {
            Gp0Op.nop => {},
            Gp0Op.draw_mode => self.draw_mode(cmd.args.draw_mode),
            _ => std.debug.panic("TODO: gpu gp0 op {x:0>2}", .{cmd.op}),
        }
    }

    // GP0(e1)
    fn draw_mode(self: *Self, args: ArgsDrawMode) void {
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

    fn gp1(self: *Self, v: u32) void {
        const cmd: Gp1Command = @bitCast(v);
        switch (cmd.op) {
            Gp1Op.reset => self.reset(),
            _ => std.debug.panic("TODO: gpu gp1 op {x:0>2}", .{cmd.op}),
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
        self.stat.set_mask_bits = false;
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
};
