`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2025/01/30 10:51:24
// Design Name: 
// Module Name: TOP_MMU
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module top (
    input  logic         clk,         // 全局时钟
    input  logic         reset_n,     // 异步复位，低有效
    input  logic [7:0]   data,        // 示例：输入数据位宽
    input  logic         data_valid,  // data 有效信号
    output logic         finish       // 表示整个运算流程完成
);

    //==============//
    // 内部连线定义 //
    //==============//
    
    // Input_Controller -> MU
    logic [7:0] element_1, element_2, element_3, element_4;
    logic       input_ready;  
    logic       start;  // Input_Controller发出的“开始运算”信号
    logic       input_start;
    
    // ROM -> MU
    logic [6:0] matrix_coe_1, matrix_coe_2;  // 示例：系数宽度8位
    logic       coe_ready;                  // 系数就绪信号

    // MU -> Controller/RAM
    logic       arithmetic_finish;
    logic [17:0] result_1, result_2, result_3, result_4;
    
    // Controller -> RAM
    logic       web;         // 写使能(或写禁用)信号，具体高低有效看设计
    logic [3:0] ram_addr;    // RAM 地址
    logic [17:0] result;      // 准备写入RAM的数据
    
    // Controller -> ROM
    logic [3:0] rom_addr; 
    logic       rom_start;

    //====================//
    // 子模块实例化及连线 //
    //====================//
    
    // 1) 输入控制模块
    input_controller u_Input_Controller (
        .clk         (clk),
        .reset_n     (reset_n),
        .input_start (input_start),  
        .data        (data),
        .data_valid  (data_valid),
        
        // 输出到 MU
        .element1   (element_1),
        .element2   (element_2),
        .element3   (element_3),
        .element4   (element_4),
        .input_ready (input_ready),
        .start       (start)
    );

    // 2) 矩阵运算单元(MU)
    MU u_MU (
        .clk             (clk),
        .reset_n         (reset_n),
        
        // 来自 Input_Controller
        .element1       (element_1),
        .element2       (element_2),
        .element3       (element_3),
        .element4       (element_4),
        
        // 来自 ROM
        .matrix_coe_1    (matrix_coe_1),
        .matrix_coe_2    (matrix_coe_2),
        .input_ready     (input_ready),
        .coe_ready       (coe_ready),

        // 输出到 Controller/RAM
        .arthmetic_finish (arithmetic_finish),
        .result_1        (result_1),
        .result_2        (result_2),
        .result_3        (result_3),
        .result_4        (result_4)
    );

    // 3) RAM存储模块
    sram_wrapper_wrapper u_RAM (
        .clk         (clk),

        // 写控制与地址
        .web         (!web),
        .ram_addr    ({4'b0,ram_addr}),
        .result      (result)    // 要写入的数据// 结果有效时可能触发写操作
        // 如需读口，可以在此处添加RAM的读数据输出等端口
    );

    // 4) 系数ROM模块
    ROM_wrapper u_ROM (
        .clk           (clk),
        .reset_n        (reset_n),
        .rom_addr      ({4'b0,rom_addr}),
        .rom_start     (rom_start),
        .matrix_coe_1  (matrix_coe_1),
        .matrix_coe_2  (matrix_coe_2),
        .coe_ready     (coe_ready)
        // 如果还需要输出busy或其他状态信号，也可增加端口
    );

    // 5) 控制器模块
    Controller u_Controller (
        .clk              (clk),
        .reset_n          (reset_n),
        
        // 来自Input_Controller或系统其他部分的开始信号
        .start            (start),
        
        // 来自 MU
        .arithmetic_finish(arithmetic_finish),
        .result1         (result_1),
        .result2         (result_2),
        .result3         (result_3),
        .result4         (result_4),
        
        // 控制 RAM
        .web              (web),
        .ram_addr         (ram_addr),
        .result           (result),
        
        // 控制 ROM
        .input_start (input_start),
        .rom_addr         (rom_addr),
        .rom_start        (rom_start),
        
        // 顶层输出：整个流程结束
        .finish           (finish)
    );

endmodule
