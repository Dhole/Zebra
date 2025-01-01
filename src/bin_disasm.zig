const std = @import("std");
const argsParser = @import("args");

const decoder = @import("decoder.zig");
const decode = decoder.decode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const Options = struct {
        bin: ?[]const u8 = null,
        offset: u32 = 0,
        help: bool = false,

        pub const shorthands = .{
            .b = "bin",
            .o = "offset",
        };

        pub const meta = .{
            .option_docs = .{
                .bin = "<BIN> Binary input path",
                .offset = "<OFFSET> Binary disassembly offset",
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

    const bin_file = try std.fs.cwd().openFile(opts_bin, .{});
    var bin_file_reader = std.io.bufferedReader(bin_file.reader());
    var buf: [4]u8 = undefined;

    const w = std.io.getStdOut().writer();
    for (0..0x10) |i| {
        const n = try bin_file_reader.read(&buf);
        if (n == 0) {
            break;
        } else if (n < 4) {
            unreachable;
        }
        const addr = i + opts.offset;
        const v = std.mem.readVarInt(u32, &buf, std.builtin.Endian.little);
        const inst = decode(v);
        try w.print("{x:0>8}: {x:0>8} {any}\n", .{ addr, v, inst });
    }
}
