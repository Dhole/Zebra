const std = @import("std");
const argsParser = @import("args");

const decoder = @import("decoder.zig");
const decode = decoder.decode;
const disasm = @import("disasm.zig");
const FmtInst = disasm.FmtInst;
const _cpu = @import("cpu.zig");
const Cpu = _cpu.Cpu;
const BIOS_SIZE = _cpu.BIOS_SIZE;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const Options = struct {
        bios: ?[]const u8 = null,
        num: u64 = 0x100000000,
        help: bool = false,

        pub const shorthands = .{
            .b = "bios",
            .n = "num",
        };

        pub const meta = .{
            .option_docs = .{
                .bios = "<BIN> BIOS file path",
                .num = "<NUM> Number of instructions to emulate",
                .help = "Print help",
            },
        };
    };
    const options = argsParser.parseForCurrentProcess(Options, allocator, .print) catch unreachable;
    defer options.deinit();
    const opts = options.options;

    if (opts.help) {
        try argsParser.printHelp(Options, options.executable_name orelse "demo", std.io.getStdOut().writer());
        return;
    }
    const opts_bios = opts.bios orelse {
        std.log.err("Missing argument 'bios'", .{});
        return;
    };

    const bios_file = try std.fs.cwd().openFile(opts_bios, .{});
    const bios = try bios_file.reader().readAllAlloc(allocator, BIOS_SIZE);
    defer allocator.free(bios);
    var cpu = try Cpu.init(allocator, bios, .{});
    defer cpu.deinit();

    const w = std.io.getStdOut().writer();
    var i: u32 = 0;
    try w.print("{}", .{&cpu});
    while (true) {
        const v = cpu.read_u32(cpu.pc);
        const inst = decode(v);
        try w.print("--------\n", .{});
        try w.print("{x:0>8}: {x:0>8} {}\n", .{ cpu.pc, v, FmtInst{ .v = inst, .pc = cpu.pc } });
        cpu.step();
        try w.print("{}", .{&cpu});

        if (@as(u64, i) == opts.num - 1) {
            break;
        }
        i += 1;
    }
}
