# 分子动力学 / Molecular Dynamics

## 定义 / Definition

分子动力学（Molecular Dynamics, MD）是一种计算机模拟方法，通过数值求解牛顿运动方程来模拟原子和分子的运动轨迹。在 CP2K 中，MD 常与 DFT 结合（Born-Oppenheimer MD 或 Car-Parrinello MD）来研究原子系统的动力学演化。

Molecular Dynamics (MD) is a computational simulation method that models the trajectories of atoms and molecules by numerically solving Newton's equations of motion. In CP2K, MD is often combined with DFT (Born-Oppenheimer MD or Car-Parrinello MD) to study the dynamical evolution of atomic systems.

## 核心机制 / Core Mechanism

### 运动方程 / Equations of Motion

经典 MD 求解牛顿方程：Classical MD solves Newton's equations:

```
F_i = m_i a_i = -∂V/∂r_i
```

对于 Born-Oppenheimer MD：For Born-Oppenheimer MD:
1. 在每个时间步求解电子结构 (DFT)
2. 计算原子受力 (Hellmann-Feynman 力)
3. 更新原子位置和速度

1. Solve electronic structure (DFT) at each timestep
2. Calculate atomic forces (Hellmann-Feynman forces)
3. Update atomic positions and velocities

### 时间积分算法 / Time Integration Algorithms

| 算法 / Algorithm | 特点 / Characteristics | 应用 / Applications |
|------------------|-------------------------|---------------------|
| Velocity Verlet | 时间可逆，辛算法，能量守恒好 | 最常用 |
| Leapfrog | 效率高，长时间稳定性好 | 大型系统 |
| RESPA | 多时间步，不同频率分离 | 多尺度系统 |

| Algorithm | Characteristics | Applications |
|-----------|------------------|---------------------|
| Velocity Verlet | Time-reversible, symplectic, good energy conservation | Most common |
| Leapfrog | Efficient, good long-term stability | Large systems |
| RESPA | Multiple timesteps, frequency separation | Multiscale systems |

### 系综和温控器 / Ensembles and Thermostats

| 系综 / Ensemble | 变量 / Variables | 温控器 / Thermostat |
|-----------------|------------------|---------------------|
| NVE (微正则) | E, V, N 守恒 | 无 |
| NVT (正则) | T, V, N 固定 | Nosé-Hoover, Langevin, Berendsen |
| NPT (等温等压) | T, P, N 固定 | Nosé-Hoover + Barostat |

| Ensemble | Variables | Thermostat |
|----------|-----------|-------------|
| NVE (Microcanonical) | E, V, N conserved | None |
| NVT (Canonical) | T, V, N fixed | Nosé-Hoover, Langevin, Berendsen |
| NPT (Isothermal-isobaric) | T, P, N fixed | Nosé-Hoover + Barostat |

### CP2K 中的 MD 设置 / MD Settings in CP2K

```cp2k
&MOTION
  &MD
    ENSEMBLE NVT
    STEPS 1000
    TIMESTEP 0.5  # fs
    TEMPERATURE 300.0  # K
    THERMOSTAT NOSE_HOOVER
  &END MD
&END MOTION
```

## 应用场景 / Applications

- **液态结构和动力学**：水的径向分布函数，扩散系数
- **生物分子模拟**：蛋白质折叠，酶催化，膜动力学
- **材料科学**：离子扩散，相变，缺陷运动
- **化学反应**：反应路径，过渡态搜索
- **表面化学**：吸附，脱附，表面反应

- **Liquid Structure and Dynamics**: Radial distribution functions of water, diffusion coefficients
- **Biomolecular Simulation**: Protein folding, enzyme catalysis, membrane dynamics
- **Materials Science**: Ion diffusion, phase transitions, defect motion
- **Chemical Reactions**: Reaction pathways, transition state search
- **Surface Chemistry**: Adsorption, desorption, surface reactions

## 相关概念 / Related Concepts

- **DFT**: 提供原子间势能
- **几何优化**: 寻找局部极小值
- **周期性边界条件**: 模拟体相系统
- **时间步长 (Timestep)**: 积分精度限制
- **温控器**: 控制系统温度

- **DFT**: Provides interatomic potential
- **Geometry Optimization**: Finding local minima
- **Periodic Boundary Conditions**: Simulating bulk systems
- **Timestep**: Integration accuracy limitation
- **Thermostats**: Controlling system temperature

## 来源 / Sources

- CP2K Input Reference: MOTION section, MD subsection
- Frenkel & Smit (2002). "Understanding Molecular Simulation"
- Tuckerman et al. (1992). "Statistical mechanics. Isobaric-isothermal molecular dynamics"
- Marx & Hutter (2009). "Ab Initio Molecular Dynamics"
