# SUBSYS 部分 / SUBSYS Section

## 概述 / Overview

`&SUBSYS` 部分定义了 CP2K 计算的子系统信息，包括原子坐标、晶胞参数、原子类型和拓扑结构。这是每个 `&FORCE_EVAL` 部分的必要组成部分。

The `&SUBSYS` section defines subsystem information for CP2K calculations, including atomic coordinates, cell parameters, atomic types, and topology. It is an essential component of each `&FORCE_EVAL` section.

## 关键属性 / Key Properties

### 基本结构 / Basic Structure

```cp2k
&SUBSYS
  &COORD
  ...
  &END COORD
  &KIND element
  ...
  &END KIND
  &CELL
  ...
  &END CELL
  &TOPOLOGY
  ...
  &END TOPOLOGY
&END SUBSYS
```

### &COORD - 原子坐标 / Atomic Coordinates

#### 内联坐标格式

```cp2k
&COORD
H     0.000000  0.000000  0.000000
H     1.000000  0.000000  0.000000
O     2.000000  1.000000  0.000000
&END COORD
```

#### 带单位的坐标

```cp2c
&COORD
C [angstrom] 1.54 2.3 1.2
H [angstrom] 2.0 2.3 1.2
&END COORD
```

#### 分子标签

```cp2k
&COORD
H  0.0  0.0  0.0  water1
H  1.0  0.0  0.0  water1
O  2.0  1.0  0.0  water1
&END COORD
```

#### 缩放坐标

```cp2k
&COORD
SCALED
F 0.0 0.0 0.0
F 0.25 0.25 0.25
&END COORD
```

### &CELL - 晶胞参数 / Cell Parameters

#### ABC 格式

```cp2k
&CELL
  ABC [angstrom] 8.0 4.0 4.0
  PERIODIC XYZ
&END CELL
```

#### ABC_ALPHA_BETA_GAMMA 格式

```cp2k
&CELL
  ABC 10.0 10.0 10.0
  ALPHA_BETA_GAMMA 90.0 90.0 90.0
  PERIODIC XYZ
&END CELL
```

#### 矢量格式

```cp2k
&CELL
  A [angstrom] 4.07419 0.0 0.0
  B [angstrom] 2.037095 3.52835204 0.0
  C [angstrom] 2.037095 1.17611735 3.32656221
  PERIODIC XYZ
&END CELL
```

#### 对称性设置

```cp2k
&CELL
  ...
  SYMMETRY ORTHORHOMBIC
&END CELL
```

### &KIND - 原子类型 / Atomic Species

#### 基本 KIND 定义

```cp2k
&KIND H
  BASIS_SET DZVP-MOLOPT-GTH
  POTENTIAL GTH-PADE-q1
&END KIND
```

#### 元素指定

```cp2k
&KIND Si
  ELEMENT Si
  BASIS_SET TZVP-MOLOPT-GTH
  POTENTIAL GTH-PBE-q4
&END KIND
```

#### 多种基组/赝势

```cp2k
&KIND C
  BASIS_SET ORB TZVP-MOLOPT-GTH
  BASIS_SET_AUX FIT ADMM_GTH_BASIS_SETS
  BASIS_SET_RI AUX_FIT RI-GTH-C3-q4
  POTENTIAL GTH-PBE-q4
&END KIND
```

#### 约束和冻结

```cp2k
&KIND C
  ...
  &FROZEN
    ALL
  &END FROZEN
&END KIND
```

### &TOPOLOGY - 拓扑结构 / Topology

#### 从文件读取

```cp2k
&TOPOLOGY
  COORD_FILE_NAME structure.xyz
  COORD_FILE_FORMAT XYZ
&END TOPOLOGY
```

#### 支持的格式

- `XYZ` - XYZ 坐标文件
- `CP2K` - CP2K 坐标格式
- `PDB` - Protein Data Bank 格式
- `CIF` - Crystallographic Information File

#### 连接信息

```cp2k
&TOPOLOGY
  CONNECTIVITY_FORMAT UC
  CONVERAGE_RADIUS 5.0
&END TOPOLOGY
```

## 相关来源 / Related Sources

### LSP 解析器

`parser.py` 中的 `coords()` 方法：
- 解析 `COORD` 部分坐标
- 单位转换处理
- 缩放坐标处理

### XML 规范

`cp2k_input.xml` 定义了 SUBSYS 相关的完整结构和验证规则。

## 常用配置模板 / Common Templates

### 周期性体系

```cp2k
&SUBSYS
  &CELL
    ABC 10.0 10.0 10.0
    PERIODIC XYZ
  &END CELL
  &COORD
  Si  0.0  0.0  0.0
  Si  0.25  0.25  0.25
  &END COORD
  &KIND Si
    BASIS_SET TZVP-MOLOPT-GTH
    POTENTIAL GTH-PBE-q4
  &END KIND
&END SUBSYS
```

### 分子体系 (气相)

```cp2k
&SUBSYS
  &CELL
    ABC 20.0 20.0 20.0
    PERIODIC NONE
  &END CELL
  &COORD
  C  0.0  0.0  0.0
  H  1.09  0.0  0.0
  H  -0.36  1.03  0.0
  H  -0.36  -0.51  0.89
  &END COORD
  &KIND C
    BASIS_SET TZVP-MOLOPT-GTH
    POTENTIAL GTH-PBE-q4
  &END KIND
  &KIND H
    BASIS_SET TZVP-MOLOPT-GTH
    POTENTIAL GTH-PBE-q1
  &END KIND
&END SUBSYS
```

### 从文件读取

```cp2k
&SUBSYS
  &CELL
    ABC 15.0 15.0 15.0
    PERIODIC XYZ
  &END CELL
  &TOPOLOGY
    COORD_FILE_NAME water.xyz
    COORD_FILE_FORMAT XYZ
  &END TOPOLOGY
  &KIND O
    BASIS_SET DZVP-MOLOPT-GTH
    POTENTIAL GTH-PBE-q6
  &END KIND
  &KIND H
    BASIS_SET DZVP-MOLOPT-GTH
    POTENTIAL GTH-PBE-q1
  &END KIND
&END SUBSYS
```

## 参考资料 / References

1. CP2K SUBSYS 参考：https://manual.cp2k.org/CP2K_INPUT/FROM_CP2K/FORCE_EVAL/SUBSYS.html
2. CP2K 坐标格式文档
3. 晶胞对称性：https://manual.cp2k.org/CP2K_INPUT/FROM_CP2K/FORCE_EVAL/SUBSYS/CELL.html

## 最佳实践 / Best Practices

1. **坐标精度**：通常使用 6-8 位小数精度
2. **真空层**：气相计算留至少 10 Å 真空层
3. **基组一致性**：确保 KIND 定义与实际元素匹配
4. **单位明确**：使用单位注记避免歧义
