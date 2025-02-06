`timescale 1ns/1ps

module ROM (
    input         clk,       // 时钟信号
    input         reset_n,   // 异步低有效复位信号
    input  [4:0]  rom_addr,  // 地址输入，共32个地址（5位）
    output reg [13:0] q      // 14位数据输出
);

    // 定义ROM存储数组：32个数据，每个数据14位
    reg [13:0] rom_mem [0:31];

    // 同步读出过程：在时钟上升沿或复位下降沿更新输出
    always @(posedge clk or negedge reset_n) begin
        if (!reset_n)
            q <= 14'b0;              // 异步复位时输出清零
        else
            q <= rom_mem[rom_addr];  // 根据地址输出对应数据
    end

endmodule
