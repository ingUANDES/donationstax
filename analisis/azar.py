"""Generador de números aleatorios común a Python y R para las simulaciones de P-05.

Con la misma semilla, azar.py y azar.R producen las mismas extracciones, de modo que las simulaciones
de los dos lenguajes trabajan sobre los mismos datos (DECISIONES.md#d-15, «Tolerancia entre Python y R para
las simulaciones»):

- MT19937 iniciado con init_genrand(semilla), el sembrado escalar de la implementación de referencia.
- Uniformes y/2^32, con y la salida de 32 bits. Es lo que devuelve runif() de R con ese estado en
  .Random.seed; un y = 0 se reemplaza por 0,5/(2^32 - 1), como hace R.
- Normales por Box-Muller: sqrt(-2 log u1) cos(2 pi u2), con u1 el primer bloque de n uniformes y u2 el
  siguiente. log y cos van por la biblioteca matemática de C (módulo math), la misma que usa R.

Las extracciones se consumen en el orden en que se piden, así que cada script debe pedirlas en el mismo
orden en los dos lenguajes.
"""
import math

import numpy as np

I2_32M1 = 2.328306437080797e-10  # 1 / (2^32 - 1), como en R
DOS_PI = 6.283185307179586


def init_genrand(semilla):
    mt = [0] * 624
    mt[0] = semilla & 0xFFFFFFFF
    for i in range(1, 624):
        mt[i] = (1812433253 * (mt[i - 1] ^ (mt[i - 1] >> 30)) + i) & 0xFFFFFFFF
    return mt


class Azar:
    def __init__(self, semilla):
        self.gen = np.random.MT19937()
        self.gen.state = {"bit_generator": "MT19937",
                          "state": {"key": np.array(init_genrand(semilla), dtype=np.uint32), "pos": 624}}

    def uniformes(self, n):
        u = self.gen.random_raw(n).astype(np.float64) / 4294967296.0
        u[u <= 0.0] = 0.5 * I2_32M1
        return u

    def normales(self, n):
        u1 = self.uniformes(n).tolist()
        u2 = self.uniformes(n).tolist()
        return np.array([math.sqrt(-2.0 * math.log(a)) * math.cos(DOS_PI * b) for a, b in zip(u1, u2)])
