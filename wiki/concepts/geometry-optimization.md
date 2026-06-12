# 几何优化 / Geometry Optimization

## 定义 / Definition

几何优化（Geometry Optimization）是通过数值方法寻找分子或晶体结构局部能量极小值的过程。在 CP2K 中，这对应于求解原子受力为零的平衡构型，是研究分子结构、反应路径和材料性质的基础。

Geometry Optimization is the process of numerically finding local energy minima of molecular or crystal structures. In CP2K, this corresponds to solving for equilibrium geometries where atomic forces are zero, forming the foundation for studying molecular structures, reaction pathways, and material properties.

## 核心机制 / Core Mechanism

### 优化目标 / Optimization Objective

寻找极小值：Find minimum:

```
min E(R)  subject to  |F_i| < F_tol
```

其中：Where:
- E(R): 势能面
- F_i = -∂E/∂R_i: 原子受力
- F_tol: 力收敛阈值

### 优化算法类型 / Optimization Algorithm Types

| 算法 / Algorithm | 类型 / Type | 特点 / Characteristics | 应用 / Applications |
|------------------|-------------|------------------------|---------------------|
| 共轭梯度 (CG) | 一阶导数 | 内存省，稳定但较慢 | 大型系统 |
| L-BFGS | 拟牛顿法 | 快速收敛，需存储历史 | 中小型系统 |
| 信赖域 (TRM) | 二阶模型 | 稳健，处理困难地形 | 复杂势能面 |
| 模式爬升 | 二阶导数 | 精确，Hessian 昂贵 | 小分子精确优化 |
| FIRE | 分子动力学型 | 适合离平衡较远的初始点 | MD 优化 |

| Algorithm | Type | Characteristics | Applications |
|------------|------|------------------------|---------------------|
| Conjugate Gradient (CG) | First-order | Memory-efficient, stable but slow | Large systems |
| L-BFGS | Quasi-Newton | Fast convergence, needs history | Small-medium systems |
| Trust Region (TRM) | Second-order model | Robust, difficult terrain | Complex potential surfaces |
| Newton-Raphson | Second-order | Precise, expensive Hessian | Small molecule precision |
| FIRE | MD-style | Good for far-from-equilibrium | MD optimization |

### 收敛准则 / Convergence Criteria

| 准则 / Criterion | CP2K 关键字 / Keyword | 典型值 / Typical Value |
|------------------|----------------------|------------------------|
| 最大力 | MAX_FORCE | 4.5E-4 ~ 4.5E-3 a.u. |
| RMS 力 | RMS_FORCE | 3.0E-4 ~ 3.0E-3 a.u. |
| 最大位移 | MAX_DISPLACEMENT | 3.0E-3 ~ 3.0E-2 a.u. |
| RMS 位移 | RMS_DISPLACEMENT | 2.0E-3 ~ 2.0E-2 a.u. |
| 能量变化 | EPS_GEO_OPT | 1.0E-6 ~ 1.0E-5 a.u. |

| Criterion | CP2K Keyword | Typical Value |
|------------|---------------|----------------|
| Maximum force | MAX_FORCE | 4.5E-4 ~ 4.5E-3 a.u. |
| RMS force | RMS_FORCE | 3.0E-4 ~ 3.0E-3 a.u. |
| Maximum displacement | MAX_DISPLACEMENT | 3.0E-3 ~ 3.0E-2 a.u. |
| RMS displacement | RMS_DISPLACEMENT | 2.0E-3 ~ 2.0E-2 a.u. |
| Energy change | EPS_GEO_OPT | 1.0E-6 ~ 1.0E-5 a.u. |

### CP2K 几何优化设置 / CP2K Geometry Optimization Settings

```cp2k
&MOTION
  &GEO_OPT
    TYPE MINIMIZATION           # 优化类型
    OPTIMIZER BFGS              # 优化算法
    MAX_ITER 100                # 最大迭代步数
    MAX_FORCE 4.5E-4           # 力收敛阈值
    RMS_FORCE 3.0E-4

    # 或更严格的收敛
    MAX_FORCE 1.0E-5
    RMS_FORCE 7.0E-5

    # 选项
    LINESEARCH 2PT             # 线搜索方法
    TRUST_RADIUS 1.0           # 信赖域半径
  &END GEO_OPT
&END MOTION

# 跑势能面扫描
&MOTION
  &GEO_OPT
    TYPE OPTIMATION
    CONSTRAINTS
      DISTANCE 1 2 1.0        # 约束键长
    END CONSTRAINTS
  &END GEO_OPT
&END MOTION
```

### 过渡态搜索 / Transition State Search

| 方法 / Method | 描述 / Description |
|---------------|--------------------|
| NEB (Nudged Elastic Band) | 路径优化，寻找最低能量路径 |
| Dimer Method | 局部鞍点搜索 |
| Eigenvector Following | 沿特定模式爬升 |
| CI-NEB | 改进 NEB，使用 climbing image |

| Method | Description |
|---------------|--------------------|
| NEB (Nudged Elastic Band) | Path optimization, minimum energy path |
| Dimer Method | Local saddle point search |
| Eigenvector Following | Climb along specific mode |
| CI-NEB | Improved NEB with climbing image |

```cp2k
&MOTION
  &GEO_OPT
    TYPE NEB
    NEB_IMAGE_COUNT 7          # 图像数量
    K_SPRING 5.0              # 弹簧常数
  &END GEO_OPT
&END MOTION
```

### 过渡态搜索 / Transition State Search

1. **初始构型很重要**: 良好的初始猜测加速收敛
2. **对称性**: 约束对称性可减少自由度
3. **约束**: 固定某些原子，优化其他部分
4. **频率验证**: 确认无虚频

1. **Initial geometry matters**: Good initial guess accelerates convergence
2. **Symmetry**: Constraining symmetry reduces degrees of freedom
3. **Constraints**: Fix some atoms, optimize others
4. **Frequency verification**: Confirm no imaginary frequencies

## 应用场景 / Applications

- **分子结构**: 确定平衡几何构型，键长，键角
- **晶体结构**: 优化晶格常数和原子位置
- **反应路径**: 反应物，产物，过渡态
- **表面吸附**: 吸附构型和位点偏好
- **材料设计**: 新结构的能量评估

- **Molecular Structure**: Equilibrium geometry, bond lengths, angles
- **Crystal Structure**: Lattice constants and atomic positions
- **Reaction Pathways**: Reactants, products, transition states
- **Surface Adsorption**: Adsorption geometry and site preference
- **Materials Design**: Energy evaluation of new structures

## 相关概念 / Related Concepts

- **势能面 (PES)**: 几何优化在其上操作
- **原子受力**: 梯度信息
- **Hessian 矩阵**: 二阶导数，曲率信息
- **振动频率**: 优化后计算验证极小值
- **分子动力学**: 另一种探索构型空间的方法

- **Potential Energy Surface (PES)**: Geometry optimization operates on it
- **Atomic Forces**: Gradient information
- **Hessian Matrix**: Second derivatives, curvature
- **Vibrational Frequencies**: Post-optimization verification of minima
- **Molecular Dynamics**: Alternative method for exploring conformational space

## 来源 / Sources

- CP2K Input Reference: MOTION section, GEO_OPT subsection
- Nocedal & Wright (2006). "Numerical Optimization"
- Henkelman & Jónsson (2000). "A climbing image nudged elastic band method"
- Page & McIver (1978). "On gradient methods in optimization"
