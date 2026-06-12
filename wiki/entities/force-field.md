# 力场方法 / Force Field Methods

## 概述 / Overview

CP2K 除了量子化学方法外，还支持经典力场计算，通过 `&FORCE_EVAL` 部分的 `METHOD CLASSICAL` 指定。力场方法适用于大规模分子动力学模拟，计算效率远高于 DFT。

In addition to quantum chemical methods, CP2K supports classical force field calculations via `METHOD CLASSICAL` in the `&FORCE_EVAL` section. Force field methods are suitable for large-scale molecular dynamics simulations with much higher computational efficiency than DFT.

## 关键属性 / Key Properties

### 力场类型 / Force Field Types

#### CLASSICAL 方法

经典力场方法，用于纯分子力学模拟：

```cp2k
&FORCE_EVAL
  METHOD CLASSICAL
  &SUBSYS
    &COORD
    ...
    &END COORD
    &KIND C
      &FORCEFIELD
        &BOND
          TYPE HARM
        &END BOND
      &END FORCEFIELD
    &END KIND
  &END SUBSYS
  &FF
    &PARAMETER
      PARMTYPE AMBER
      PAR_FILE_NAME prmtop
    &END PARAMETER
  &END FF
&END FORCE_EVAL
```

#### QM/MM 混合方法

量子力学/分子力学混合方法：

```cp2k
&FORCE_EVAL
  METHOD QMMM
  &SUBSYS
    &QM_KIND
      C
    &END QM_KIND
    &MM_KIND
      H
    &END MM_KIND
  &END SUBSYS
&END FORCE_EVAL
```

### 力场格式支持 / Supported Force Field Formats

#### AMBER 力场

```cp2k
&FF
  &PARAMETER
    PARMTYPE AMBER
    PAR_FILE_NAME amber.parm7
    COORD_FILE_NAME amber.rst7
  &END PARAMETER
&END FF
```

#### CHARMM 力场

```cp2k
&FF
  &PARAMETER
    PARMTYPE CHARMM
    PAR_FILE_NAME charmm.prm
  &END PARAMETER
&END FF
```

#### GROMOS 力场

```cp2k
&FF
  &PARAMETER
    PARMTYPE GROMOS
    PAR_FILE_NAME gromos.prm
  &END PARAMETER
&END FF
```

### FIST 模块

FIST (Fast Integrated Simulation Tool) 是 CP2K 的经典力场引擎：

```cp2k
&FORCE_EVAL
  METHOD FIST
  &FIST
    TEMPERATURE 300.0
    PRESSURE 1.0
    TIME_STEP 1.0
  &END FIST
&END FORCE_EVAL
```

## 相关来源 / Related Sources

### LSP 解析器

`parser.py` 中力场相关的解析：
- `&FORCE_EVAL` 部分
- `METHOD CLASSICAL` 关键字
- `&FF` 部分解析
- `&FIST` 部分解析

### XML 规范

`cp2k_input.xml` 定义了力场相关的完整结构。

## 力场参数设置 / Force Field Parameters

### 原子类型定义

```cp2k
&KIND C
  &FORCEFIELD
    &BOND
      TYPE HARM
      K 300.0
      R0 1.54
    &END BOND
    &ANGLE
      TYPE HARM
      K 50.0
      THETA0 109.5
    &END ANGLE
    &DIHEDRAL
      TYPE COS
      K 1.0
      N 1
      PHI0 180.0
    &END DIHEDRAL
    &NONBONDED
      SIGMA 3.4
      EPSILON 0.086
    &END NONBONDED
  &END FORCEFIELD
&END KIND
```

### 非键相互作用

```cp2k
&NONBONDED
  &LENNARD-JONES
    TYPE C6
  &END LENNARD-JONES
  &ELECTROSTATIC
    COUPLING EWALD
    EWALD_TYPE EW3D
    ALPHA 0.5
    GMAX 12
  &END ELECTROSTATIC
&END NONBONDED
```

## 应用场景 / Applications

| 场景 | 推荐方法 | 说明 |
|-----|---------|------|
| 生物分子 | AMBER/CHARMM | 蛋白质、核酸模拟 |
| 材料科学 | GROMOS | 分子晶体、聚合物 |
| 溶剂化体系 | TIP3P/TIP4P | 水溶液模拟 |
| 界面体系 | QM/MM | 表面吸附、反应 |

## 参考资料 / References

1. AMBER 力场：http://ambermd.org/
2. CHARMM 力场：https://www.charmm.org/
3. CP2K 力场文档：https://manual.cp2k.org/CP2K_INPUT/FROM_CP2K/FORCE_EVAL/MM/FIST.html

## 相关工具 / Related Tools

- `parmck2` - AMBER 参数文件转换
- `pdb2gmx` - GROMACS 格式转换
- `acpype` - Antechamber Python 界面
