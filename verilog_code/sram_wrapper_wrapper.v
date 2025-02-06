`timescale 1ns/1ps
//************************************************************
// 模块名称：sram_writer
// 功能：封装 sram_wrapper，仅用于写数据到 SRAM
//       输入端口：
//         - clk      : 时钟信号
//         - ram_addr : 地址信号（8位）
//         - web      : 写使能信号（低有效）
//         - result   : 要写入的数据（32位）
//       输出端口：无（sram_wrapper 的读数据和状态信号内部处理）
//************************************************************
module sram_wrapper_wrapper(
    input             clk,
    input      [7:0]  ram_addr,
    input             web,     // 写使能，低有效
    input      [17:0] result   // 要写入的数据
);

    // 内部连接：sram_wrapper 的输出（本模块不对外输出）
    wire [31:0] read_data;
    wire        ry;

    // 实例化 sram_wrapper 模块
    // 注意：cs_n 固定为低电平，表示芯片始终使能
    sram_wrapper u_sram_wrapper (
        .clk        (clk),
        .cs_n       (1'b0),      // 固定为低：始终使能芯片
        .we_n       (web),       // 写使能信号，低有效
        .address    (ram_addr),  // 地址信号
        .ry         (ry),        // 状态信号（内部连接，不对外输出）
        .write_data ({14'b0,result}),    // 写数据
        .read_data  (read_data)  // 读数据（内部连接，不对外输出）
    );

endmodule
