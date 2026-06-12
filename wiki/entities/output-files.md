# 输出文件格式 / Output File Formats

## 概述 / Overview

CP2K 计算会生成多种输出文件，包括主输出文件、重启文件、轨迹文件和结构文件。这些文件包含计算结果、中间状态和分析数据。

CP2K calculations generate various output files, including the main output file, restart files, trajectory files, and structure files. These files contain calculation results, intermediate states, and analysis data.

## 关键属性 / Key Properties

### 主输出文件 / Main Output File

#### .out 文件

标准输出文件，包含：
- 计算参数摘要
- SCF 收敛信息
- 能量、力和应力信息
- 原子坐标和速度
- 警告和错误信息

文件名默认为 `PROJECT_NAME.out`

```
CP2K| version 2024.1
...
ENERGY| Total FORCE_EVAL ( F= -2.3473674278E+02 E= -2.3473674278E+02 )
...
```

### 重启文件 / Restart Files

#### .restart 文件

包含计算完整状态，用于：
- 断点续算
- 作为后续计算的输入
- 波函数分析

文件类型：
- `PROJECT_NAME-1.restart` - 第一次计算的重启文件
- `PROJECT_NAME-2.restart` - 第二次计算的重启文件

```cp2k
&EXT_RESTART
  RESTART_FILE_NAME previous_calc.restart
  &RESTART
    WAVEFUNCTION
  &END RESTART
&END EXT_RESTART
```

### 轨迹文件 / Trajectory Files

#### XYZ 轨迹

```cp2k
&MD
  &PRINT
    &TRAJECTORY
      FORMAT XYZ
      STRIDE 10
    &END TRAJECTORY
  &END PRINT
&END MD
```

生成 `PROJECT_NAME-pos-1.xyz`

#### DCD 轨迹

```cp2k
&TRAJECTORY
  FORMAT DCD
&END TRAJECTORY
```

### 结构文件 / Structure Files

#### 优化后结构

几何优化后输出：
- `PROJECT_NAME-optimization.xyz` - 优化轨迹
- `PROJECT_NAME-pos-1.xyz` - 最终结构

### 振动分析输出 / Vibrational Analysis Outputs

振动分析计算生成：
- `PROJECT_NAME-vib_modes-1.cif` - 振动模式 CIF 格式
- `PROJECT_NAME-vib_modes-1.molden` - Molden 格式可视化
- `PROJECT_NAME-vib_freq.dat` - 振动频率数据

### 能量数据文件 / Energy Data Files

#### .ener 文件

存储每步计算的能量：

```cp2k
&ENERGY
  &ENERGY_OUTPUT
    FORMAT HIGH
  &END ENERGY_OUTPUT
&END ENERGY
```

生成 `PROJECT_NAME-1.ener`

### 其他输出文件 / Other Output Files

#### 波函数文件

- `PROJECT_NAME.wfn` - 波函数数据
- `PROJECT_NAME.mo` - 分子轨道
- `PROJECT_NAME.bse` - Bethe-Salpeter 数据

#### 密度文件

- `PROJECT_NAME-ELECTRON_DENSITY-1.cube` - 电子密度 Cube 格式

#### 力和应力文件

```cp2k
&PRINT
  &FORCES
    FORMAT XYZ
  &END FORCES
  &STRESS_TENSOR
  &END STRESS_TENSOR
&END PRINT
```

## 相关来源 / Related Sources

### LSP 解析器

`parser.py` 中的 `coords()` 方法：
- 从重启文件读取坐标
- 单位转换处理
- 坐标格式解析

### 文件处理工具

CP2K 提供的工具：
- `cp2kget` - 从重启文件提取数据
- `xyz2cp2k` - XYZ 格式转换
- `cp2k-rstchk` - 重启文件检查

## 文件命名约定 / File Naming Conventions

CP2K 使用以下命名模式：

- `PROJECT_NAME-xxx.ext`
  - `xxx` 是文件类型编号
  - `ext` 是文件扩展名

示例：
- `water-1.restart` - 第一次计算的重启文件
- `water-pos-1.xyz` - 位置轨迹文件
- `water-ener-1.dat` - 能量数据文件

## 常用输出控制 / Common Output Control

### 轨迹输出

```cp2k
&MD
  &PRINT
    &TRAJECTORY
      FORMAT XYZ
      STRIDE 10
      ENSURE_TRAJECTORY_CONTINUITY
    &END TRAJECTORY
    &VELOCITIES
      FORMAT XYZ
      STRIDE 100
    &END VELOCITIES
  &END PRINT
&END MD
```

### 能量输出

```cp2k
&MOTION
  &MD
    ...
  &END MD
  &PRINT
    &ENERGY
      STRIDE 1
    &END ENERGY
  &END PRINT
&END MOTION
```

### 调试输出

```cp2k
&GLOBAL
  PRINT_LEVEL DEBUG
&END GLOBAL
```

## 参考资料 / References

1. CP2K 输出文件参考：https://manual.cp2k.org/
2. VMD 可视化工具：https://www.ks.uiuc.edu/Research/vmd/
3. Molden：http://www.cmbi.ru.nl/molden/

## 相关工具 / Related Tools

### 可视化软件

- **VMD** - 分子可视化
- **VESTA** - 晶体结构可视化
- **Jmol** - 交互式 3D 可视化
- **Avogadro** - 分子编辑器

### 分析工具

- **MDAnalysis** - Python 轨迹分析
- **MDTraj** - 分子动力学轨迹分析
- **cclib** - 量子化学输出解析

## 最佳实践 / Best Practices

1. **文件管理**：使用有意义的 PROJECT_NAME
2. **输出频率**：根据存储容量调整 STRIDE
3. **压缩存储**：定期压缩归档输出文件
4. **备份重要**：保留关键计算的 .restart 文件
5. **轨迹检查**：定期检查轨迹文件完整性
