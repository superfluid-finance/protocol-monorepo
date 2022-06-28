#!/usr/bin/env sage -python3
# -*- coding: utf-8 -*-
# Part of Superfluid Contracts. See LICENSE file for full copyright and licensing details.


"""
This module defines and solves the symbolic equations for the Decaying Flow Agreement (DFA), including:

- defining rtb equation,
- solving aad_mempty_update_with_acd equation,
- solving aad_mappend equation.
"""

from sage.all import e, var, assume, solve, oo

lambda_0 = var("lambda_0")
assume(lambda_0 > 0)

class AAD:
    """DFA Agreement Account Data Type Definition"""

    def __init__(self, suffix = ""):
        """AAD is a monoid, its constructor is the mempty"""
        self.alpha = var("alpha" + suffix)
        self.epsilon = var("epsilon" + suffix)
        self.settledAt = var("t_s" + suffix)

class ACD:
    """DFA Agreement Account Data Type Definition"""

    def __init__(self, suffix = ""):
        self.distributionLimit = var("theta" + suffix)
        self.updatedAt = var("t" + suffix)

def rtb(aad: AAD, t: var):
    """Short hand for the providedBalanceOfAgreement function"""
    alpha = aad.alpha
    epsilon = aad.epsilon
    settledAt = aad.settledAt
    return alpha * e ** (- lambda_0 * (t - settledAt)) + epsilon

def solve_aad_mempty_update_with_acd(newACD: ACD) -> AAD:
    print("# Solve aad_mempty_update_with_acd equation\n")
    print("Given:")
    aad = AAD()
    t = var("t")
    print("\tvariables: ", t)
    print("\tdata: newACD (θ = {}, t_u = {})".format(newACD.distributionLimit, newACD.updatedAt))
    # Initial account state
    print("\trtb(aad, t) = ", rtb(aad, t))
    print("\n")
    # New account state
    solution = solve(
          [ rtb(aad, newACD.updatedAt) == 0
          , rtb(aad, t).limit(t = oo) == - newACD.distributionLimit
          , aad.settledAt == newACD.updatedAt],
          [aad.alpha, aad.epsilon, aad.settledAt])
    assert(len(solution) == 1)
    print("Solution:\n\t", solution[0])
    aad_prime = AAD()
    aad_prime.alpha = aad.alpha.subs(solution[0][0])
    aad_prime.epsilon = aad.epsilon.subs(solution[0][1])
    aad_prime.settledAt = aad.settledAt.subs(solution[0][2])
    print("\trtb(aad_prime, t) = ", rtb(aad_prime, t))
    print("\n")
    return aad_prime

def solve_aad_mappend(aad1: AAD, aad2: AAD) -> AAD:
    print("# Solve aad_mappend equation\n")
    print("Given:")
    t = var("t")
    print("\tvariables: ", t)
    print("\tdata: aad1, aad2")
    # Initial account state
    print("\trtb(aad1, t) = ", rtb(aad1, t))
    print("\trtb(aad2, t) = ", rtb(aad2, t))
    # Next account state
    aad_prime = AAD("_prime")
    print("\trtb(aad_prime, t) = ", rtb(aad_prime, t))
    print("\n")
    # New account state
    equition0 = rtb(aad_prime, t) == rtb(aad1, t) + rtb(aad2, t)
    print("Search solution to:\n\trtb(aad_prime, t) == rtb(aad1, t) + rtb(aad2, t)\n\t==>\n\t"
        + str(equition0))
    print("\n")
    # Undetermined Solution(s)
    solution1 = solve(equition0, aad_prime.alpha)
    assert(len(solution1) == 1)
    print("Solution 1:\n\t", solution1[0])
    print("\n")
    # Solution
    print("Solution Final:")
    print("\tlet:", aad_prime.settledAt == aad2.settledAt)
    aad_prime.settledAt = aad2.settledAt
    print("\tlet:", aad_prime.epsilon == aad1.epsilon + aad2.epsilon)
    aad_prime.epsilon = aad1.epsilon + aad2.epsilon
    equition1 = rtb(aad_prime, t) == rtb(aad1, t) + rtb(aad2, t)
    solution = solve(equition1, aad_prime.alpha)
    assert(len(solution) == 1)
    print("\tsolved:", solution[0])
    print("\n")

def solve_half_life():
    print("# Solve DFA.RTB Half-life Period\n")
    t_0 = var("t_0")
    t_h = var("t_h")
    theta = var("theta")
    aad = AAD()
    aad.settledAt = t_0
    aad.alpha = theta
    aad.epsilon = -theta
    solution = solve(-theta == 2 * rtb(aad, t_0 + t_h), t_h)
    print("Solution:\n\t", solution[0])
    print("\n")

if __name__=="__main__":
    acd1 = ACD("_1")
    aad1 = solve_aad_mempty_update_with_acd(acd1)

    solve_aad_mappend(AAD(), aad1)

    solve_half_life()
