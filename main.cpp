#include <iostream>
#include <vector>
#include <array>
#include <chrono>
#include <random>
#include <algorithm>
#include <numeric>

#include "kaizen.h"

int main(int argc, char* argv[]){
    // Parse command line arguments
    zen::cmd_args args(argv, argc);

    // Check for required arguments: --N and --M.
    if (!args.is_present("--N") || !args.is_present("--M")) {
        std::cerr << "Usage: " << argv[0] 
                  << " --N <number_of_particles> --M <num_iterations>" 
                  << std::endl;
        return 1;
    }

    int N = std::stoi(args.get_options("--N")[0]);
    int M = std::stoi(args.get_options("--M")[0]);
    if (N <= 0 || M <= 0) {
        std::cerr << "Number of particles and iterations must be positive integers." 
                  << std::endl;
        return 1;
    }

    // Random number generator
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dis(-1.0, 1.0);

    // Array of Structs (AoS)
    struct Particle {
        double x, y, z;
        double px, py, pz;
        double spin;
    }; 
    std::vector<Particle> particles(N);
    for (int i = 0; i < N; i++){
        particles[i].x = dis(gen);
        particles[i].y = dis(gen);
        particles[i].z = dis(gen);
        particles[i].px = dis(gen);
        particles[i].py = dis(gen);
        particles[i].pz = dis(gen);
        particles[i].spin = dis(gen);
    }

    // Struct of Arrays (SoA)
    struct Particles {
        std::vector<double> x, y, z;
        std::vector<double> px, py, pz;
        std::vector<double> spin;
    };
    Particles particles_soa;
    particles_soa.x.resize(N);
    particles_soa.y.resize(N);
    particles_soa.z.resize(N);
    particles_soa.px.resize(N);
    particles_soa.py.resize(N);
    particles_soa.pz.resize(N);
    particles_soa.spin.resize(N);
    for (int i = 0; i < N; i++){
        particles_soa.x[i] = dis(gen);
        particles_soa.y[i] = dis(gen);
        particles_soa.z[i] = dis(gen);
        particles_soa.px[i] = dis(gen);
        particles_soa.py[i] = dis(gen);
        particles_soa.pz[i] = dis(gen);
        particles_soa.spin[i] = dis(gen);
    }

    // Summing phases
    auto start = std::chrono::high_resolution_clock::now();
    double sum_spin = 0.0;
    for (int i = 0; i < N; i++){
        sum_spin += particles[i].spin;
    }
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed_seconds = end - start;
    std::cout << "AoS sum_spin: " << sum_spin << " in " << elapsed_seconds.count() << "s" << std::endl;

    start = std::chrono::high_resolution_clock::now();
    sum_spin = 0.0;
    for (int i = 0; i < N; i++){
        sum_spin += particles_soa.spin[i];
    }
    end = std::chrono::high_resolution_clock::now();
    elapsed_seconds = end - start;
    std::cout << "SoA sum_spin: " << sum_spin << " in " << elapsed_seconds.count() << "s" << std::endl;

    // Updating spin states
    start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < N; i++){
        particles[i].spin = dis(gen);
    }
    end = std::chrono::high_resolution_clock::now();
    elapsed_seconds = end - start;
    std::cout << "AoS update_spin: " << elapsed_seconds.count() << "s" << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < N; i++){
        particles_soa.spin[i] = dis(gen);
    }
    end = std::chrono::high_resolution_clock::now();
    elapsed_seconds = end - start;
    std::cout << "SoA update_spin: " << elapsed_seconds.count() << "s" << std::endl;

    // Memory usage
    std::cout << "AoS memory usage: " << sizeof(Particle) * N << " bytes" << std::endl;
    std::cout << "SoA memory usage: " << sizeof(double) * 6 * N << " bytes" << std::endl;

    // Iteration speed
    start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < M; i++){
        for (int j = 0; j < N; j++){
            particles[j].x += particles[j].px;
            particles[j].y += particles[j].py;
            particles[j].z += particles[j].pz;
        }
    }
    end = std::chrono::high_resolution_clock::now();
    elapsed_seconds = end - start;
    std::cout << "AoS iteration speed: " << elapsed_seconds.count() << "s" << std::endl;

    start = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < M; i++){
        for (int j = 0; j < N; j++){
            particles_soa.x[j] += particles_soa.px[j];
            particles_soa.y[j] += particles_soa.py[j];
            particles_soa.z[j] += particles_soa.pz[j];
        }
    }
    end = std::chrono::high_resolution_clock::now();
    elapsed_seconds = end - start;
    std::cout << "SoA iteration speed: " << elapsed_seconds.count() << "s" << std::endl;

    return 0; 
}


    // Cache efficiency
    // The Struct of Arrays (SoA) layout is more cache-efficient than the Array of Structs (AoS) layout.
    // In the AoS layout, all properties of a particle are stored together in memory,
    // which can lead to cache misses when iterating over the particles.
    // In the SoA layout, each property of all particles is stored together in memory,
    // which can lead to better cache utilization when iterating over the particles.
    // This optimization matters in large-scale quantum physics simulations because
    // the performance of the simulation can be significantly affected by memory access patterns
    // and cache utilization, especially when dealing with a large number of particles. 

    // In conclusion, the Struct of Arrays (SoA) layout is more cache-efficient than the Array of Structs (AoS) layout,
    // and this optimization can improve the performance of large-scale quantum physics simulations by reducing memory access times and cache misses.   
