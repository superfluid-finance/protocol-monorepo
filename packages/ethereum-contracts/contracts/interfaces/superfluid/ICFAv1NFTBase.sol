// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.4;

interface ICFAv1NFTBase {

    // FlowData struct storage packing:
    // b = bits
    // WORD 1: | flowSender     | flowStartDate | FREE
    //         | 160b           | 32b           | 64b
    // WORD 2: | flowReceiver   | FREE      
    //         | 160b           | 96b       
    // @note Using 32 bits for flowStartDate is future proof "enough":
    // 2 ** 32 - 1 = 4294967295 seconds
    // Will overflow after: Sun Feb 07 2106 08:28:15
    struct FlowData {
        address flowSender;
        uint32 flowStartDate;
        address flowReceiver;
    }
}