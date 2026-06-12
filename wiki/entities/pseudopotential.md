# 赝势 / Pseudopotential

## 概述 / Overview

赝势 (Pseudopotential, PP) 是用于简化电子结构计算的有效方法，通过将内层电子替换为有效势，减少计算量。CP2K 支持多种赝势类型。

Pseudopotentials (PP) are effective methods to simplify electronic structure calculations by replacing core electrons with an effective potential, reducing computational cost. CP2K supports multiple pseudopotential types.

## 关键属性 / Key Properties

### 赝势类型 / Pseudopotential Types

#### GTH 赝势

Goedecker-Teter-Hutter (GTH) 赝势，CP2K 的标准赝势：

- `GTH-PADE-q1` - 氢原子 GTH 赝势
- `GTH-PADE-q6` - 碳原子 GTH 赝势
- `GTH-BLYP` - BLYP 泛函优化
- `GTH-PBE` - PBE 泛函优化
- `GTH-PBE0` - PBE0 混合泛函

#### PAW 赝势

投影缀加波 (Projector Augmented Wave) 方法：

- 更高的精度，但计算成本更高
- 支持元素周期表大部分元素

#### SG 赝势

Soft Gaussian 赝势，用于某些特定场景。

### 使用方式 / Usage

在 `&KIND` 部分指定赝势：

```cp2k
&KIND H
  POTENTIAL GTH-PADE-q1
&END KIND

&KIND C
  POTENTIAL GTH-PBE-q6
&END KIND

&KIND O
  POTENTIAL GTH-PBE-q6
&END KIND
```

### 赝势文件 / Pseudopotential File

通过 `POTENTIAL_FILE_NAME` 指定赝势文件路径：

```cp2k
&DFT
  POTENTIAL_FILE_NAME ./POTENTIALS
&END DFT
```

赝势文件格式示例：
```
# Hydrogen (GTH-PADE-q1)
H    GTH-PADE-q1
1
1 0 0 2
0.0 0.0
0.3616188390 0.0
0.6138068526 0.0
```

## 相关来源 / Related Sources

### LSP 解析器

`parser.py` 中赝势相关的解析：
- `POTENTIAL` 关键字解析
- `POTENTIAL_FILE_NAME` 关键字解析
- `KIND` 部分中的赝势定义

### 验证工具

`cp2k-datafile-lint` 可用于验证赝势文件格式。

## 赝势选择指南 / Pseudopotential Selection Guide

| 元素 | 推荐赝势 | 说明 |
|-----|--------|------|
| H | GTH-PADE-q1, GTH-PBE-q1 | 氢原子 |
| He | GTH-PADE-q2, GTH-PBE-q2 | 氦原子 |
| Li-Ne | GTH-PBE-q1 到 q8 | 第一、二周期 |
| 过渡金属 | GTH-PBE 系列 | 根据价电子数选择 |

## 与基组配合 / Compatibility with Basis Sets

赝势必须与基组配合使用：

```cp2k
&KIND Si
  BASIS_SET TZVP-MOLOPT-GTH
  POTENTIAL GTH-PBE-q4
&END KIND
```

注意：确保基组和赝势的命名约定一致 (如 GTH)。

## 参考资料 / References

1. Goedecker, S. & Teter, M. & Hutter, J. "Separable dual-space Gaussian pseudopotentials" (1996)
2. CP2K 赝势库：https://www.cp2k.org/potentials
3. PAW 方法：https://doi.org/10.1103/PhysRevB.59.17583

## 相关文件 / Related Files

- `cp2k-datafile-lint` - 赝势文件格式验证工具
- 赝势文件通常位于 `POTENTIALS` 目录
