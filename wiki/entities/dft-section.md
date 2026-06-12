# DFT 部分 / DFT Section

## 概述 / Overview

`&DFT` 部分是 CP2K 中配置密度泛函理论计算的核心部分，包含了 DFT 计算的所有主要参数设置。

The `&DFT` section is the core part for configuring Density Functional Theory calculations in CP2K, containing all major parameter settings for DFT computations.

## 关键属性 / Key Properties

### 基本设置 / Basic Settings

```cp2k
&DFT
  BASIS_SET_FILE_NAME ./BASIS_SETS
  POTENTIAL_FILE_NAME ./POTENTIALS
  LSD                        # 自旋极化
  CHARGE 0.0                 # 系统总电荷
  MULTIPLICITY 1             # 多重度
&END DFT
```

### 主要子部分 / Main Subsections

#### &MGRID - 多网格设置

控制网格精度和计算效率：

```cp2k
&MGRID
  CUTOFF 140                # 平面波截断能 (Ry)
  REL_CUTOFF 70             # 相对截断能
  NGRIDS 4                  # 多网格层数
  COMMENSURATE             # 使用相称网格
  PROGRESSIVE_LEVELS       # 渐进式网格
&END MGRID
```

#### &QS - 快速多极子方法

```cp2k
&QS
  EPS_DEFAULT 1.0E-8        # 默认精度
  EPS_PGF_ORB 1.0E-6       # 轨道精度
  METHOD GAPW              # GAPW 方法
&END QS
```

#### &SCF - 自洽场设置

```cp2k
&SCF
  EPS_SCF 1.0E-6           # SCF 收敛阈值
  MAX_SCF 50               # 最大 SCF 迭代次数
  SCF_GUESS atomic         # 初始猜测类型
  ADDED_MOS 40             # 额外分子轨道数
  &OT
    METHOD DIIS            # 密度混合方法
    PRECONDITIONER FULL_ALL
  &END OT
  &SMEAR
    METHOD FERMI_DIRAC     # 费米-狄拉克展宽
    ELECTRONIC_TEMPERATURE 300.0
  &END SMEAR
&END SCF
```

#### &XC - 交换关联泛函

```cp2k
&XC
  DENSITY_CUTOFF 400       # 密度截断
  GRADIENT_CUTOFF 400      # 梯度截断
  &XC_FUNCTIONAL PBE       # 泛函类型
  &END XC_FUNCTIONAL
  &VDW_POTENTIAL
    POTENTIAL_TYPE PPF     # 范德华校正
    DISPERSION_FUNCTIONAL PBE
  &END VDW_POTENTIAL
&END XC
```

### 泛函选择 / Functional Selection

#### LDA 泛函
- `LDA` - 局域密度近似
- `LDA_X` + `LDA_C_PZ` - Perdew-Zunger 参数化

#### GGA 泛函
- `PBE` - Perdew-Burke-Ernzerhof (最常用)
- `BLYP` - Becke-Lee-Yang-Parr
- `revPBE` - 修订版 PBE

#### 混合泛函
- `PBE0` - PBE 混合泛函
- `B3LYP` - Becke 3 参数混合泛函

#### 范德华校正
- `DFT-D3` - Grimme D3 校正
- `DFT-D4` - Grimme D4 校正
- `PPF` - 通用密度泛函修正

## 相关来源 / Related Sources

### LSP 解析器

`parser.py` 中 DFT 相关的解析逻辑：
- `&DFT` 部分解析
- `&XC` 部分解析
- `&SCF` 部分解析
- `&MGRID` 部分解析

### XML 规范

`cp2k_input.xml` 中定义了完整的 DFT 参数结构和验证规则。

## 常用配置模板 / Common Templates

### 标准 PBE 计算

```cp2k
&DFT
  BASIS_SET_FILE_NAME BASIS_SET
  POTENTIAL_FILE_NAME POTENTIAL
  &MGRID
    CUTOFF 400
    REL_CUTOFF 60
  &END MGRID
  &XC
    &XC_FUNCTIONAL PBE
    &END XC_FUNCTIONAL
  &END XC
  &SCF
    EPS_SCF 1.0E-6
    MAX_SCF 100
  &END SCF
&END DFT
```

### 自旋极化计算

```cp2k
&DFT
  LSD
  MULTIPLICITY 3
  UKS                       # 非限制 Kohn-Sham
  ...
&END DFT
```

### 混合泛函计算

```cp2k
&DFT
  &XC
    &XC_FUNCTIONAL PBE0
    &END XC_FUNCTIONAL
    &HF
      FRACTION 0.25
      SCREENING POTENTIAL
      &INTERACTION_POTENTIAL
        POTENTIAL_TYPE TRUNCATED
        CUTOFF 6.0
      &END INTERACTION_POTENTIAL
    &END HF
  &END XC
&END DFT
```

## 参考资料 / References

1. Hohenberg, P. & Kohn, W. "Inhomogeneous Electron Gas" (1964)
2. Kohn, W. & Sham, L. "Self-Consistent Equations" (1965)
3. Perdew, J.P. et al. "Generalized Gradient Approximation" (1996)
4. CP2K DFT 参考：https://manual.cp2k.org/CP2K_INPUT/FROM_CP2K/FORCE_EVAL/DFT.html

## 性能优化建议 / Performance Optimization

1. **截断能选择**：根据基组选择合适的 CUTOFF
2. **多网格设置**：NGRIDS 通常设为 4-6
3. **SCF 加速**：使用 DIIS 或 OT 方法
4. **并行化**：调整并行网格参数
