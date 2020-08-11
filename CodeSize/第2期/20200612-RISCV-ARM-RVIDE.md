

# 1介绍

本文主要针对 “RISCV 工具链数据报第二期：CodeSize ARM vs RISCV 和 RISCV IDE 评测” 复现实验过程。实验过程分为两部分，第一部分是基于 GNU toolchain 和 CSiBE 对 ARM 和 RISCV 的codesize进行测试和对比；第二部分是对 RISCV IDE 的测评。

# 2 Part1 ARM 和 RISCV 的 codesize测试和对比

测评使用的 RISCV32 和 RISCV64 的 toolchain，同第一期所用。

测试对照组和编译选项依照数据报第二期，相应的`.cmake` 文件附在`/20200612-RISCV-ARM-RVIDE-data/codes/` 目录下，得到的 codesize 测试结果如下：

表1：12个测试用例平均每个文件的text段字节数绝对值

![figure_os_o3os](D:\Eternal-Balance\第2期\figure_os_o3os.png)

![figure_risc64_aarch64](D:\Eternal-Balance\第2期\figure_risc64_aarch64.png)

![figure_x32_x16](D:\Eternal-Balance\第2期\figure_x32_x16.png)

上述结果同数据报第二期结果有细微差别，但总体趋势类似；差别比较大的是 AARCH64 对比 RISCV64 位指令集下 codesize 的膨胀比例，-O3选项下，AARCH64 的codesize 平均比 RISCV64 大37%（数据报中为27%）；-O3 -Os选项下，AARCH64 的codesize 平均比 RISCV64 大111%（数据报中为3%）。



# 3 Part 2 RISCV IDE

测试方法：使用相同的编译选项对比 RISCV IDE 内嵌的toolchain 与 GNU 官方相同基础版本的 codesize 指标。

评测的目标程序：

-  dhrystone



GNU GCC：

|        | RISC IDE |      |      |      | GNU  |      |      |      |
| ------ | -------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
|        | o2       | o2os | o3   | o3os | o2   | o2os | o3   | o3os |
| dhry_1 | 3983     | 3559 | 4007 | 3559 | 3831 | 3439 | 3851 | 3439 |
| dhry_2 | 264      | 248  | 240  | 248  | 256  | 242  | 236  | 242  |

