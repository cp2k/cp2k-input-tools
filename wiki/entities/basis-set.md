# 基组 / Basis Set

## 概述 / Overview

基组 (Basis Set) 是量子化学中用于描述分子轨道的数学函数集合。CP2K 支持多种基组类型，用于不同精度的计算。

Basis sets are collections of mathematical functions used to describe molecular orbitals in quantum chemistry. CP2K supports multiple basis set types for different calculation accuracies.

## 关键属性 / Key Properties

### 基组类型 / Basis Set Types

#### Gaussian 基组

- **DZVP** - 双-zeta 价极化基组
- **TZVP** - 三-zeta 价极化基组
- **QZVP** - 四-zeta 价极化基组
- **SZV** - 单-zeta 价基组

#### MOLOPT 基组

专为分子动力学优化的基组：
- `DZVP-MOLOPT-GTH` - 双-zeta MOLOPT 基组
- `TZVP-MOLOPT-GTH` - 三-zeta MOLOPT 基组
- `TZV2P-MOLOPT-GTH` - 增强型三-zeta MOLOPT 基组

#### PADE 基组

- `DZV-GTH-PADE` - 双-zeta PADE 基组
- `TZV-GTH-PADE` - 三-zeta PADE 基组

#### ADMM 基组

- 辅助密度矩阵方法基组
- 用于加速 DFT 计算

### 使用方式 / Usage

在 `&KIND` 部分指定基组：

```cp2k
&KIND H
  BASIS_SET DZVP-MOLOPT-GTH
&END KIND

&KIND O
  BASIS_SET TZVP-MOLOPT-GTH
&END KIND
```

或指定不同类型的基组：

```cp2k
&KIND C
  BASIS_SET ORB TZVP-MOLOPT-GTH
  BASIS_SET_AUX FIT ADMM_GTH_BASIS_SETS
  BASIS_SET_RI AUX_FIT RI-GTH-C3-q4
&END KIND
```

### 基组文件 / Basis Set File

通过 `BASIS_SET_FILE_NAME` 指定基组文件路径：

```cp2k
&DFT
  BASIS_SET_FILE_NAME ./BASIS_SETS
&END DFT
```

基组文件格式示例：
```
# Hydrogen
H    DZVP-MOLOPT-GTH
1
2
0.25000000
    4.4745062951    0.3026997216
    0.8804080775    0.6718908896
```

## 相关来源 / Related Sources

### LSP 解析器

`parser.py` 中基组相关的解析：
- `BASIS_SET` 关键字解析
- `BASIS_SET_FILE_NAME` 关键字解析
- `KIND` 部分中的基组定义

### XML 规范

`cp2k_input.xml` 定义了基组相关的数据结构和验证规则。

## 基组选择指南 / Basis Set Selection Guide

| 应用场景 | 推荐基组 | 说明 |
|---------|---------|------|
| 分子动力学 | DZVP-MOLOPT-GTH | 平衡精度与效率 |
| 高精度计算 | TZVP-MOLOPT-GTH | 更高精度 |
| 大规模系统 | SZV | 快速初步计算 |
| 混合计算 | ADMM | 加速 DFT 计算 |

## 参考资料 / References

1. VandeVondele, J. & Hutter, J. "Gaussian basis sets for accurate calculations on molecular systems" (2007)
2. CP2K 基组库：https://www.cp2k.org/basis_sets
3. MOLOPT 基组论文：https://doi.org/10.1063/1.3459040

## 相关文件 / Related Files

- `cp2k-datafile-lint` - 基组文件格式验证工具
- 基组文件通常位于 `BASIS_SETS` 目录
