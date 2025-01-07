const std = @import("std");
const eql = std.mem.eql;
const argsParser = @import("args");

const decoder = @import("decoder.zig");
const decode = decoder.decode;
const disasm = @import("disasm.zig");
const print_disasm = disasm.print_disasm;
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
        cmd: ?[]const u8 = null,
        help: bool = false,

        pub const shorthands = .{
            .b = "bios",
            .c = "cmd",
        };

        pub const meta = .{
            .option_docs = .{
                .bios = "<BIN> BIOS file path",
                .cmd = "<CMD> Initial list of commands, separated by `;`",
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

    var init_cmd_it = std.mem.tokenizeSequence(u8, opts.cmd orelse "", ";");

    var last_cmd: Cmd = .nop;
    loop: while (true) {
        try w.print("> ", .{});
        var buf_input: ?[]const u8 = undefined;
        if (init_cmd_it.next()) |cmd_str| {
            buf_input = cmd_str;
        } else {
            buf_input = try r.readUntilDelimiterOrEof(&buf, '\n');
        }

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
            .@"continue" => {
                cpu.dbg_run();
            },
            .disasm => |a| {
                try disasm_block(w, &cpu, cpu.pc, a.n);
            },
            .disasm_at => |a| {
                try disasm_block(w, &cpu, a.addr, a.n);
            },
            .@"break" => |a| {
                for (cpu.dbg.breaks.items) |addr| {
                    if (addr == a.addr) {
                        try w.print("ERR: Duplicate breakpoint\n", .{});
                        continue :loop;
                    }
                }
                try cpu.dbg.breaks.append(a.addr);
            },
            .delete_break => |a| {
                for (cpu.dbg.breaks.items, 0..) |addr, i| {
                    if (addr == a.addr) {
                        _ = cpu.dbg.breaks.orderedRemove(i);
                        continue :loop;
                    }
                }
                try w.print("ERR: Breakpoint not found\n", .{});
            },
            .info_breaks => {
                for (cpu.dbg.breaks.items, 0..) |addr, i| {
                    try w.print("break {d} at {x:0>8}\n", .{ i, addr });
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
    @"continue",
    @"break": struct { addr: u32 },
    delete_break: struct { addr: u32 },
    info_breaks,
    disasm: struct { n: u32 },
    disasm_at: struct { addr: u32, n: u32 },
    regs,
    help,
    unknown,
    nop,
};

// it: *std.mem.TokenIterator
fn parse_bool(it: anytype) !bool {
    const arg = it.next() orelse return error.MissingArgument;
    if (eql(u8, arg, "t")) {
        return true;
    } else if (eql(u8, arg, "f")) {
        return false;
    } else {
        return error.InvalidArgument;
    }
}

// it: *std.mem.TokenIterator
fn parse_word(it: anytype) !u32 {
    const arg = it.next() orelse return error.MissingArgument;
    return std.fmt.parseInt(u32, arg, 16) catch error.InvalidArgument;
}

// it: *std.mem.TokenIterator
fn parse_int_or(comptime T: type, it: anytype, default: T) !T {
    const arg = it.next() orelse return default;
    return std.fmt.parseInt(T, arg, 0) catch error.InvalidArgument;
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
    } else if (eql(u8, cmd, "c")) {
        return .@"continue";
    } else if (eql(u8, cmd, "ib")) {
        return .info_breaks;
    } else if (eql(u8, cmd, "b")) {
        const addr = try parse_word(&it);
        return .{ .@"break" = .{ .addr = addr } };
    } else if (eql(u8, cmd, "db")) {
        const addr = try parse_word(&it);
        return .{ .delete_break = .{ .addr = addr } };
    } else if (eql(u8, cmd, "trace_inst")) {
        const value = try parse_bool(&it);
        return .{ .trace_inst = .{ .v = value } };
    } else if (eql(u8, cmd, "s")) {
        const n = try parse_int_or(usize, &it, 1);
        return .{ .step = .{ .n = n } };
    } else if (eql(u8, cmd, "l")) {
        const n = try parse_int_or(u32, &it, 10);
        return .{ .disasm = .{ .n = n } };
    } else if (eql(u8, cmd, "la")) {
        const addr = try parse_word(&it);
        const n = try parse_int_or(u32, &it, 10);
        return .{ .disasm_at = .{ .addr = addr, .n = n } };
    } else {
        return .unknown;
    }
}

fn print_help(w: anytype) !void {
    try w.print(
        \\Commands:
        \\ h : Print help
        \\ s (n) : Step `n` or 1 instruction
        \\ c : Continue runnig until the next breakpoint
        \\ r : Print CPU Registers
        \\ trace_inst {{t,f}} : Enable/disable tracing all executed instructions
        \\ b [addr] : Add breakpoint at `addr`
        \\ db [addr] : Delete breakpoint at `addr`
        \\ ib : (Info) List all breakpoints
        \\ l (n) : List disassembly of `n` or 10 instructions at PC
        \\ la [addr] (n) : List disassembly of `n` or 10 instructions at `addr`
        \\ d [addr] (n) : Dump `n` or 256 bytes of memory at `addr`
        \\
    , .{});
}

// cpu: *Cpu
fn disasm_block(writer: anytype, cpu: anytype, addr: u32, n: u32) !void {
    for (0..n) |i| {
        const i_u32: u32 = @intCast(i);
        const a: u32 = addr + i_u32 * 4;
        const inst_raw = cpu.read(u32, a);
        const inst = decode(inst_raw);
        try print_disasm(writer, a, inst_raw, inst);
    }
}
