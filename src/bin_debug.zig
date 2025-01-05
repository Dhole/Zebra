const std = @import("std");
const eql = std.mem.eql;
const argsParser = @import("args");

const decoder = @import("decoder.zig");
const decode = decoder.decode;
const disasm = @import("disasm.zig");
const FmtInst = disasm.FmtInst;
const _cpu = @import("cpu.zig");
const Cpu = _cpu.Cpu;
const Cfg = _cpu.Cfg;
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

    const w = std.io.getStdOut().writer();
    const r = std.io.getStdIn().reader();
    var buf: [128]u8 = undefined;

    const cfg = Cfg{ .dbg = true };
    var cpu = try Cpu(@TypeOf(w), cfg).init(allocator, bios, w);
    defer cpu.deinit();

    var last_cmd: Cmd = .nop;
    while (true) {
        try w.print("> ", .{});
        const buf_input = try r.readUntilDelimiterOrEof(&buf, '\n');
        const input = buf_input orelse break;
        const cmd = parse_input(input) catch |err| {
            switch (err) {
                error.InvalidArgument => try w.print("ERR: Invalid argument\n", .{}),
                error.MissingArgument => try w.print("ERR: Missing argument\n", .{}),
            }
            continue;
        } orelse last_cmd;

        switch (cmd) {
            .trace_inst => |a| {
                cpu.dbg.trace_inst = a.v;
            },
            .step => |a| {
                for (0..a.n) |_| {
                    cpu.step();
                }
            },
            .regs => try cpu.format_regs(w),
            .help => try print_help(w),
            .unknown => {
                try w.print("ERR: Unknown command\n", .{});
            },
            .nop => {},
        }
        last_cmd = cmd;
    }
}

const Cmd = union(enum) {
    step: struct { n: usize },
    trace_inst: struct { v: bool },
    regs,
    help,
    unknown,
    nop,
};

fn parse_bool(arg: []const u8) !bool {
    if (eql(u8, arg, "t")) {
        return true;
    } else if (eql(u8, arg, "f")) {
        return false;
    } else {
        return error.InvalidArgument;
    }
}

fn parse_input(input: []const u8) !?Cmd {
    var it = std.mem.tokenizeSequence(u8, input, " ");
    const cmd = it.next() orelse {
        return null;
    };
    if (eql(u8, cmd, "h")) {
        return .help;
    } else if (eql(u8, cmd, "r")) {
        return .regs;
    } else if (eql(u8, cmd, "trace_inst")) {
        const value = try parse_bool(it.next() orelse return error.MissingArgument);
        return .{ .trace_inst = .{ .v = value } };
    } else if (eql(u8, cmd, "s")) {
        const n = if (it.next()) |n_str|
            std.fmt.parseInt(usize, n_str, 0) catch {
                return error.InvalidArgument;
            }
        else
            1;
        return .{ .step = .{ .n = n } };
    } else {
        return .unknown;
    }
}

fn print_help(w: anytype) !void {
    try w.print(
        \\Commands:
        \\ h : Print help
        \\ s [n] : Step `n` or 1 instruction
        \\ trace_inst {{t,f}} : Enable/disable tracing all executed instructions
        \\ r : Print CPU Registers
        \\
    , .{});
}
