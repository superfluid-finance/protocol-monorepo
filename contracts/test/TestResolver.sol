/* solhint-disable not-rely-on-time */
pragma solidity 0.6.6;

contract TestResolver {

    mapping(string => address) private _registry;

    function set(string calldata name, address target) external {
        _registry[name] = target;
    }

    function get(string calldata name) external view returns (address) {
        return _registry[name];
    }

}
