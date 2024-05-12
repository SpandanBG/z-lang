const std = @import("std");

const lexer = @import("lexer.zig");
const token = @import("token.zig");

const prompt = ">> ";

pub fn Start() !void {
    const in = std.io.getStdIn().reader();
    const out = std.io.getStdOut().writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var tknzr = try token.Tokenizer.init(allocator);
    defer tknzr.deinit();

    var lxr: lexer.Lexer = undefined;

    while (true) {
        _ = try out.write(prompt);
        const input = try in.readUntilDelimiterAlloc(allocator, '\n', 4096);
        if (std.mem.eql(u8, "exit", input)) break;

        var fb = std.io.fixedBufferStream(input);
        lxr = try lexer.Lexer.init(fb.reader().any(), &tknzr, allocator);

        while (true) {
            const tkn = try lxr.next_token();
            if (tkn.t_type == token.TokenType.EOF) break;

            _ = try out.print("Type = {s}, Literal = {s}\n", .{ token.debug_str(tkn.t_type), tkn.literal });
        }

        lxr.deinit();
    }
}
