#!/usr/bin/env sage -python
# -*- coding: utf-8 -*-
# Part of Superfluid Contracts. See LICENSE file for full copyright and licensing details.

"""
This module uses sagemath to solve the equations for both uniswap and flowswap
type of exchanges.
"""

from sage.all import var, function, solve
from sage.symbolic.integration.integral import definite_integral

def CLP(initialLa, initialLb, newLa, newLb):
    """
    @description CLP : Constant Liquidity Product
    """
    return newLa * newLb == initialLa * initialLb

def solveUniswap():
    """
    @description Solve uniswap equations:

    - Output function
    - Price function
    """
    print "==== Uniswap Equations ===="
    L_a, Delta_a, L_b, Delta_b = var("L_a","Delta_a", "L_b", "Delta_b")

    liquidityEquation = CLP(
        L_a,
        L_b,
        L_a + Delta_a,
        L_b + Delta_b
    )
    print "Liquidity Equation: \t", liquidityEquation

    solutions_Delta_b = solve(liquidityEquation, Delta_b)
    assert len(solutions_Delta_b) == 1
    Delta_b1 = solutions_Delta_b[0].right()
    print "Output Function:    \t", Delta_b1

    Price = - Delta_b1 / Delta_a
    print "Price Function:     \t", Price

def solveFlowswap():
    """
    @description Solve uniswap equations:

    - q function
    - Price function
    - Output function (TODO)
    """
    print "==== Flowswap Equations ===="
    L_a, L_b, T, t, q = var("L_a", "L_b", "T", "t", "q")
    f_a = function("f_a", nargs = 1)
    f_b = function("f_b", nargs = 1)

    liquidityEquation = CLP(
        L_a,
        L_b,
        L_a + f_a(t) + q * f_b(t),
        L_b + f_b(t) + 1/q * f_a(t)
    )
    print "Liquidity Equation: \t", liquidityEquation

    solutions_q = solve(liquidityEquation, q)
    print "q Function:         \t", solutions_q

    q1 = solutions_q[0].right()
    Delta_a = f_a(t) + q1 * f_b(t)
    Delta_b = f_b(t) + 1/q1 * f_a(t)
    Price = - Delta_b / Delta_a
    print "Price Function:     \t", Price

solveUniswap()
solveFlowswap()
