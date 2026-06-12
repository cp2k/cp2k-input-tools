# 周期性边界条件 / Periodic Boundary Conditions

## 定义 / Definition

周期性边界条件（Periodic Boundary Conditions, PBC）是一种模拟技术，用于模拟无限延伸的周期性系统。通过将模拟盒子在所有方向（或选定方向）上重复，消除了表面效应，使有限系统可以代表体相材料。

Periodic Boundary Conditions (PBC) is a simulation technique used to model infinitely extended periodic systems. By replicating the simulation box in all directions (or selected directions), surface effects are eliminated, allowing a finite system to represent bulk materials.

## 核心机制 / Core Mechanism

### 基本原理 / Basic Principle

对于盒子向量 L = (Lx, Ly, Lz)：For box vectors L = (Lx, Ly, Lz):

```
r_i' = r_i + n · L
```

其中 n = (nx, ny, nz) 是整数向量，r_i' 是周期像。

Where n = (nx, ny, nz) is an integer vector, r_i' is a periodic image.

### 最小像约定 / Minimum Image Convention

计算粒子间距离时：When calculating interparticle distances:

```
r_ij = min|r_i - r_j + n · L|
```

选择最短的周期距离，确保物理正确性。

Choose the shortest periodic distance for physical correctness.

### PBC 维度 / PBC Dimensions

| 类型 / Type | 维度 / Dimensions | 应用 / Applications |
|-------------|-------------------|---------------------|
| 3D PBC | XYZ 全周期 | 晶体，液体，体相材料 |
| 2D PBC (板状) | XY 周期，Z 自由 | 表面，薄膜，片层材料 |
| 1D PBC (棒状) | X 周期，YZ 自由 | 纳米管，聚合物链 |
| 0D (孤立) | 无周期 | 分子，团簇 |

| Type | Dimensions | Applications |
|------|------------|---------------------|
| 3D PBC | XYZ periodic | Crystals, liquids, bulk |
| 2D PBC (Slab) | XY periodic, Z free | Surfaces, thin films, sheets |
| 1D PBC (Rod) | X periodic, YZ free | Nanotubes, polymer chains |
| 0D (Isolated) | No periodicity | Molecules, clusters |

### CP2K 中的 PBC 设置 / PBC Settings in CP2K

```cp2k
&SUBSYS
  &CELL
    ABC 8.0 4.0 4.0           # 正交盒子
    # 或
    ABC 10.0 10.0 10.0
    PERIODIC XYZ              # 3D 周期
    # 或
    PERIODIC XY               # 2D 板状
  &END CELL
&END SUBSYS

&FORCE_EVAL
  &SUBSYS
    &CELL
      ABC 8.0 8.0 8.0
    &END CELL
  &END SUBSYS
  &DFT
    &POISSON
      PERIODIC XYZ            # 与 CELL 匹配
      POISSON_SOLVER PERIODIC
    &END POISSON
  &END DFT
&END FORCE_EVAL
```

### 泊松求解器 / Poisson Solvers

| 求解器 / Solver | 类型 / Type | 适用场景 / Use Case |
|-----------------|-------------|---------------------|
| PERIODIC | FFT, 3D 周期 | 3D 晶体 |
| MT (Martyna-Tuckerman) | 2D 板状校正 | 表面，2D 材料 |
| MULTIGRID | 多网格，通用 | 复杂几何 |

| Solver | Type | Use Case |
|--------|------|----------|
| PERIODIC | FFT, 3D periodic | 3D crystals |
| MT (Martyna-Tuckerman) | 2D slab correction | Surfaces, 2D materials |
| MULTIGRID | Multigrid, general | Complex geometry |

### 长程相互作用 / Long-Range Interactions

- **Ewald 求和**: 处理周期性库仑相互作用
- **PME (Particle Mesh Ewald)**: FFT 加速的 Ewald
- **截断半径**: 必须小于 L/2，避免自相互作用

- **Ewald Summation**: Handle periodic Coulomb interactions
- **PME (Particle Mesh Ewald)**: FFT-accelerated Ewald
- **Cutoff Radius**: Must be < L/2 to avoid self-interaction

## 应用场景 / Applications

- **晶体模拟**: 金属、半导体、离子晶体
- **液体模拟**: 体相水、溶液、熔盐
- **表面科学**: 吸附、催化、表面反应
- **纳米材料**: 碳纳米管、石墨烯、MOF
- **生物系统**: 膜蛋白（周期性方向）

- **Crystal Simulation**: Metals, semiconductors, ionic crystals
- **Liquid Simulation**: Bulk water, solutions, molten salts
- **Surface Science**: Adsorption, catalysis, surface reactions
- **Nanomaterials**: Carbon nanotubes, graphene, MOFs
- **Biological Systems**: Membrane proteins (periodic direction)

## 相关概念 / Related Concepts

- **k 点采样**: 倒易空间积分
- **布里渊区**: 周期系统的动量空间
- **超胞**: 扩大原胞以容纳缺陷
- **镜像原子**: 周期边界产生的虚拟原子
- **真空层**: 2D PBC 中的隔离空间

- **k-points Sampling**: Reciprocal space integration
- **Brillouin Zone**: Momentum space of periodic systems
- **Supercell**: Enlarged unit cell for defects
- **Image Atoms**: Virtual atoms from periodic boundaries
- **Vacuum Layer**: Separation in 2D PBC

## 来源 / Sources

- CP2K Input Reference: CELL section, POISSON section
- /Users/yhm/Desktop/code/cp2k-lsp-enhanced/tests/inputs/He_PBE.inp (lines 24-26)
- Allen & Tildesley (1987). "Computer Simulation of Liquids"
- Martyna & Tuckerman (1999). "A reciprocal space based method"
