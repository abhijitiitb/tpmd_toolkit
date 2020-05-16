#!/bin/bash

#PBS -N AGTrimer

#PBS -q intel

#PBS -l nodes=10:ppn=16

#PBS -j oe

#PBS -V

cd $PBS_O_WORKDIR

mpirun -np 16 ./lmp_mpi < in.sivac > out.sivac
