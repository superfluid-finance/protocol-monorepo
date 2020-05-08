pragma solidity 0.6.6;


contract InstruEventsDebug {

   event Int(int256 value);
   event Uint(uint256 value);
   event String(string value);
   event Bytes(bytes value);
   event Address(address value);

   event LogInt(string, int256);
   event LogUint(string, uint256);
   event LogString(string, string);
   event LogBytes(string, bytes data);
   event LogAddress(string, address);
}
