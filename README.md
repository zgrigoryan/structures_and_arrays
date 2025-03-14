# Quantum Particle Simulation using AoS vs. SoA

## Overview
This project simulates thousands of quantum particles and evaluates the performance of two different data layouts:

1. **Array of Structs (AoS)** - Each particle's properties (position, momentum, spin) are stored together in a struct.
2. **Struct of Arrays (SoA)** - Each property of all particles is stored separately in its own array.

By comparing the **iteration speed, memory usage, and update efficiency**, this project demonstrates the impact of memory layout on computational performance, particularly in large-scale physics simulations.

## Kaizen Library Usage
This project utilizes the **Kaizen** library for command-line argument parsing. The library makes it easy to handle input parameters and ensures correctness when specifying simulation settings.

### Installation

### Command-Line Usage
To run the simulation, use:
```bash
./main --N <number_of_particles> --M <num_iterations>
```
For example, to simulate **1000 particles** over **100 iterations**:
```bash
./main --N 1000 --M 100
```

## Tests Performed
The following tests were conducted to compare **AoS vs. SoA**:

1. **Summing Spin Values**
   - Measures the time taken to iterate over all particles and sum their spin values.
2. **Updating Spin States**
   - Assigns new random spin values to each particle and measures the update time.
3. **Memory Usage Calculation**
   - Compares memory consumption between AoS and SoA layouts.
4. **Iteration Speed**
   - Measures the time taken to update particle positions over multiple iterations.

## Expected Output
Here is an example output of the program with **N = 1000** and **M = 100**:
```plaintext
AoS sum_spin: -14.1348 in 8.334e-06s
SoA sum_spin: 14.0745 in 8.375e-06s
AoS update_spin: 6.6958e-05s
SoA update_spin: 6.5958e-05s
AoS memory usage: 56000 bytes
SoA memory usage: 48000 bytes
AoS iteration speed: 0.00132175s
SoA iteration speed: 0.00133229s
```

## Performance Analysis
### **Cache Efficiency**
- **Array of Structs (AoS)** stores all properties of a particle together in memory.
  - This can lead to **cache misses** when iterating over a single property (e.g., summing all spins).
- **Struct of Arrays (SoA)** stores each property in a contiguous array.
  - This improves **cache locality** and enables **SIMD optimizations**, making property-wise operations faster.

### **Why It Matters in Quantum Simulations**
- Large-scale quantum physics simulations involve millions of particles.
- Optimized memory access patterns **reduce execution time and memory overhead**.
- SoA is especially beneficial in **GPU-based computing** due to its efficient memory access.

## Conclusion
- **SoA is more cache-efficient than AoS**, leading to better performance in large-scale simulations.
- **For small-scale problems, the performance difference may be negligible.**
- **For millions of particles, SoA significantly reduces memory access times and cache misses.**

This project demonstrates the importance of **data layout optimization** in scientific computing, particularly in quantum physics and high-performance computing (HPC) applications.

