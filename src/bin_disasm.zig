const std = @import("std");
const argsParser = @import("args");

const decoder = @import("decoder.zig");
const decode = decoder.decode;
const disasm = @import("disasm.zig");
const FmtInst = disasm.FmtInst;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const Options = struct {
        bin: ?[]const u8 = null,
        offset: ?u32 = null,
        map: u32 = 0,
        num: u64 = 0x100000000,
        help: bool = false,

        pub const shorthands = .{
            .b = "bin",
            .o = "offset",
            .m = "map",
            .n = "num",
        };

        pub const meta = .{
            .option_docs = .{
                .bin = "<BIN> Binary input path",
                .map = "<ADDRESS> Position to map the binary",
                .offset = "<ADDRESS> Mapped binary offset to disassemble",
                .num = "<NUM> Number of instructions to disassemble",
                .help = "Print help",
            },
        };
    };
    const options = argsParser.parseForCurrentProcess(Options, gpa.allocator(), .print) catch unreachable;
    defer options.deinit();
    const opts = options.options;

    if (opts.help) {
        try argsParser.printHelp(Options, options.executable_name orelse "demo", std.io.getStdOut().writer());
        return;
    }
    const opts_bin = opts.bin orelse {
        std.log.err("Missing argument 'bin'", .{});
        return;
    };
    const opts_offset = opts.offset orelse opts.map;

    const bin_file = try std.fs.cwd().openFile(opts_bin, .{});
    try bin_file.seekTo(opts_offset - opts.map);
    var bin_file_reader = std.io.bufferedReader(bin_file.reader());
    var buf: [4]u8 = undefined;

    const w = std.io.getStdOut().writer();
    var i: u32 = 0;
    while (true) {
        const n = try bin_file_reader.read(&buf);
        if (n == 0) {
            break;
        } else if (n < 4) {
            unreachable;
        }
        const addr = opts_offset + i * 4;
        const v = std.mem.readVarInt(u32, &buf, std.builtin.Endian.little);
        const inst = decode(v);
        try w.print("{x:0>8}: {x:0>8} {}", .{ addr, v, FmtInst{ .v = inst, .pc = addr } });
        try w.print("\n", .{});

        if (@as(u64, i) == opts.num - 1) {
            break;
        }
        i += 1;
    }
}
