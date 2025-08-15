const std = @import("std");

// GPU Registers

pub const Stat = packed struct {
    // Depth of the pixel values in a texture page
    pub const TextureDepth = enum(u2) {
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
    pub const DmaDirection = enum(u2) {
        off = 0,
        fifo = 1,
        cpu_to_gp0 = 2,
        vram_to_cpu = 3,
    };

    // Video output vertical resolution
    pub const VerticalRes = enum(u1) {
        // 240 lines
        n_240 = 0,
        // 480 lines (only available for interlaced output)
        n_480 = 1,
    };

    pub const HorizontalRes = enum {
        n_256,
        n_320,
        n_512,
        n_640,
        n_386,
    };

    // Video Modes
    pub const VideoMode = enum(u1) {
        /// NTSC: 480i60H
        ntsc = 0,
        /// PAL: 576i50Hz
        pal = 1,
    };

    // Display area color depth
    pub const DisplayDepth = enum(u1) {
        /// 15 bits per pixel
        n_15bits = 0,
        /// 24 bits per pixel
        n_24bits = 1,
    };

    // Interlaced output splits each frame in two fields
    pub const Field = enum(u1) {
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

    pub fn init() Self {
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

    pub fn get(self: *Self) u32 {
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
    pub fn horizontal_res(self: *Self) HorizontalRes {
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

pub const Xy16 = packed struct {
    x: u8,
    y: u8,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 16);
    }
};

pub const Color = packed struct {
    red: u8,
    green: u8,
    blue: u8,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 24);
    }
};

pub const Xy = packed struct {
    x: u16,
    y: u16,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }
};

pub const Gp0HalfPacket = packed union {
    xy: Xy16,
    raw: u16,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 16);
    }
};

pub const Gp0Packet = packed union {
    pub const Color32 = packed struct {
        v: Color,
        unused: u8,
        comptime {
            std.debug.assert(@bitSizeOf(@This()) == 32);
        }
    };
    pub const LoHi = packed struct {
        lo: Gp0HalfPacket,
        hi: Gp0HalfPacket,
        comptime {
            std.debug.assert(@bitSizeOf(@This()) == 32);
        }
    };

    lo_hi: LoHi,
    xy: Xy,
    color: Color32,
    comptime {
        std.debug.assert(@bitSizeOf(@This()) == 32);
    }
};

pub const Gp0Cmd = packed struct {
    pub const Op = enum(u8) {
        // GP0(00h) - NOP (?)
        nop = 0x00,
        // GP0(01h) - Clear Cache
        clear_cache = 0x01,
        // GP0(28h) - Monochrome four-point polygon, opaque
        quad_mono_opaque = 0x28,
        // GP0(2Ch) - Textured four-point polygon, opaque, texture-blending
        quad_texture_blend_opaque = 0x2c,
        // GP0(30h) - Shaded three-point polygon, opaque
        triangle_shaded_opaque = 0x30,
        // GP0(38h) - Shaded four-point polygon, opaque
        quad_shaded_opaque = 0x38,
        // GP0(A0h) - Copy Rectangle (CPU to VRAM)
        image_load = 0xa0,
        // GP0(C0h) - Copy Rectangle (VRAM to CPU)
        image_store = 0xc0,
        // GP0(E1h) - Draw Mode setting (aka "Texpage")
        draw_mode = 0xe1,
        // GP0(E2h) - Texture Window setting
        texture_window = 0xe2,
        // GP0(E3h) - Set Drawing Area top left (X1,Y1)
        drawing_area_top_left = 0xe3,
        // GP0(E4h) - Set Drawing Area bottom right (X2,Y2)
        drawing_area_bottom_right = 0xe4,
        // gP0(E5h) - Set Drawing Offset (X,Y)
        drawing_offset = 0xe5,
        // GP0(E6h) - Mask Bit Setting
        bit_mask_setting = 0xe6,
        _,
    };

    pub const Args = packed union {
        pub const DrawMode = packed struct {
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

        pub const DrawingArea = packed struct {
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

        pub const DrawingOffset = packed struct {
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

        pub const TextureWindow = packed struct {
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

        pub const BitMaskSetting = packed struct {
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

        color: Color,
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

pub const Gp1Cmd = packed struct {
    pub const Op = enum(u8) {
        // GP1(00h) - Reset GPU
        reset = 0x00,
        // GP1(01h) - Reset Command Buffer
        reset_command_buffer = 0x01,
        // GP1(02h) - Acknowledge GPU Interrupt (IRQ1)
        acknowledge_irq = 0x02,
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

    pub const Args = packed union {
        pub const DisplayMode = packed struct {
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

        pub const DirectionArgs = packed struct {
            // 0-1  DMA Direction (0=Off, 1=FIFO, 2=CPUtoGP0, 3=GPUREADtoCPU) ;GPUSTAT.29-30
            dma_direction: Stat.DmaDirection,
            // 2-23 Not used (zero)
            unused: u22,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        pub const DisplayVramStart = packed struct {
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

        pub const DisplayHorizontalRange = packed struct {
            // 0-11   X1 (260h+0)       ;12bit       ;\counted in 53.222400MHz units,
            x1: u12,
            // 12-23  X2 (260h+320*8)   ;12bit       ;/relative to HSYNC
            x2: u12,
            comptime {
                std.debug.assert(@bitSizeOf(@This()) == 24);
            }
        };

        pub const DisplayVerticalRange = packed struct {
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

        pub const DisplayEnable = packed struct {
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
