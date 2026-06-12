# MOTION 部分 / MOTION Section

## 概述 / Overview

`&MOTION` 部分控制 CP2K 中的原子运动计算，包括几何优化、分子动力学、蒙特卡洛和振动分析等。通过 `RUN_TYPE` 全局关键字或 `&MOTION` 部分指定计算类型。

The `&MOTION` section controls atomic motion calculations in CP2K, including geometry optimization, molecular dynamics, Monte Carlo, and vibrational analysis. The calculation type is specified via the `RUN_TYPE` global keyword or the `&MOTION` section.

## 关键属性 / Key Properties

### 计算类型 / Calculation Types

#### 几何优化 (GEO_OPT)

```cp2k
&GLOBAL
  RUN_TYPE GEO_OPT
&END GLOBAL

&MOTION
  &GEO_OPT
    OPTIMIZER BFGS
    MAX_ITER 200
    RMS_FORCE 1.0E-4
    MAX_FORCE 3.0E-4
  &END GEO_OPT
&END MOTION
```

优化算法选项：
- `BFGS` - Broyden-Fletcher-Goldfarb-Shanno (默认)
- `LBFGS` - 有限内存 BFGS
- `CG` - 共轭梯度
- `NEWTON` - 牛顿法

#### 分子动力学 (MD)

```cp2k
&GLOBAL
  RUN_TYPE MD
&END GLOBAL

&MOTION
  &MD
    ENSEMBLE NVT
    STEPS 10000
    TIMESTEP 0.5
    TEMPERATURE 300.0
    &THERMOSTAT
      TYPE NOSE
      REGION MOLECULAR
    &END THERMOSTAT
  &END MD
&END MOTION
```

系综类型：
- `NVE` - 微正则系综
- `NVT` - 正则系综 (需恒温器)
- `NPT` - 等温等压系综 (需恒温器+恒压器)
- `NPT_F` - 柔性体积 NPT

#### 单点能量计算 (ENERGY)

```cp2k
&GLOBAL
  RUN_TYPE ENERGY
&END GLOBAL
```

#### 能量和力计算 (ENERGY_FORCE)

```cp2k
&GLOBAL
  RUN_TYPE ENERGY_FORCE
&END GLOBAL
```

#### 振动分析 (VIBRATIONAL_ANALYSIS)

```cp2k
&GLOBAL
  RUN_TYPE VIBRATIONAL_ANALYSIS
&END GLOBAL

&MOTION
  &VIBRATIONAL_ANALYSIS
    DO_EIGENVECTORS .TRUE.
    EPS_EIGENVECTOR 1.0E-6
  &END VIBRATIONAL_ANALYSIS
&END MOTION
```

#### 蒙特卡洛 (MC)

```cp2k
&GLOBAL
  RUN_TYPE MC
&END GLOBAL

&MOTION
  &MC
    MOVES 10000
    TEMPERATURE 300.0
    DISPLACEMENT 0.2
  &END MC
&END MOTION
```

#### 晶胞优化 (CELL_OPT)

```cp2k
&GLOBAL
  RUN_TYPE CELL_OPT
&END GLOBAL

&MOTION
  &CELL_OPT
    CELL_OPTIMIZER BFGS
    PRESSURE 1.0
  &END CELL_OPT
&END MOTION
```

### MD 特定设置 / MD-Specific Settings

#### 恒温器 / Thermostat

```cp2k
&THERMOSTAT
  TYPE NOSE
  REGION MOLECULAR
  TIMECON 1000
&END THERMOSTAT
```

恒温器类型：
- `NOSE` - Nosé-Hoover 链
- `CSVR` - Canonical Sampling through Velocity Rescaling
- `LANGEVIN` - 兰之万动力学
- `BERENDSEN` - Berendsen 弱耦合

#### 恒压器 / Barostat

```cp2k
&BAROSTAT
  TYPE PARRINELLO-RAHMAN
  TIMECON 1000
  PRESSURE 1.0
&END BAROSTAT
```

恒压器类型：
- `PARRINELLO-RAHMAN` - Parrinello-Rahman 方法
- `MTK` - Martyna-Klein-Tuckerman

#### 约束 / Constraints

```cp2k
&CONSTRAINT
  &COLLECTIVE
    TYPE DISTANCE
    ATOMS 1 2
    TARGET 1.54
  &END COLLECTIVE
&END CONSTRAINT
```

#### 输出控制 / Output Control

```cp2k
&MD
  ...
  &PRINT
    &TRAJECTORY
      FORMAT XYZ
      ENSURE_TRAJECTORY_CONTINUITY
    &END TRAJECTORY
    &VELOCITIES
      FORMAT XYZ
    &END VELOCITIES
    &FORCES
      FORMAT XYZ
    &END FORCES
  &END PRINT
&END MD
```

## 相关来源 / Related Sources

### LSP 解析器

`parser.py` 中 MOTION 相关的解析：
- `RUN_TYPE` 关键字解析
- `&MOTION` 部分解析
- `&MD`、`&GEO_OPT` 等子部分解析

### XML 规范

`cp2k_input.xml` 定义了完整的 MOTION 参数结构。

## 常用配置模板 / Common Templates

### 标准 NVT 分子动力学

```cp2k
&GLOBAL
  RUN_TYPE MD
&END GLOBAL

&MOTION
  &MD
    ENSEMBLE NVT
    STEPS 100000
    TIMESTEP 0.5
    TEMPERATURE 300.0
    &THERMOSTAT
      TYPE CSVR
      REGION MOLECULAR
      TIMECON 1000
    &END THERMOSTAT
    &PRINT
      &TRAJECTORY
        FORMAT XYZ
        STRIDE 10
      &END TRAJECTORY
    &END PRINT
  &END MD
&END MOTION
```

### 几何优化

```cp2k
&GLOBAL
  RUN_TYPE GEO_OPT
&END GLOBAL

&MOTION
  &GEO_OPT
    OPTIMIZER BFGS
    MAX_ITER 500
    RMS_FORCE 1.0E-4
    MAX_FORCE 3.0E-4
  &END GEO_OPT
&END MOTION
```

### NPT 系综模拟

```cp2k
&GLOBAL
  RUN_TYPE MD
&END GLOBAL

&MOTION
  &MD
    ENSEMBLE NPT
    STEPS 100000
    TIMESTEP 0.5
    TEMPERATURE 300.0
    PRESSURE 1.0
    &THERMOSTAT
      TYPE NOSE
    &END THERMOSTAT
    &BAROSTAT
      TYPE PARRINELLO-RAHMAN
    &END BAROSTAT
  &END MD
&END MOTION
```

## 参考资料 / References

1. Nosé, S. "A unified formulation of the constant temperature molecular dynamics methods" (1984)
2. Parrinello, M. & Rahman, A. "Polymorphic transitions in single crystals" (1981)
3. CP2K MOTION 参考：https://manual.cp2k.org/CP2K_INPUT/FROM_CP2K/MOTION.html

## 性能优化建议 / Performance Optimization

1. **时间步长**：通常 0.5-1.0 fs，根据键刚性调整
2. **输出频率**：适当设置 STRIDE 减少输出
3. **约束**：使用 SHAKE/RATTLE 允许更大时间步长
4. **并行化**：大规模系统需调整并行策略
