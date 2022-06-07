module.exports = {
    printParams(params) {
        return params
            .map((p) => (p.name ? `${p.type} ${p.name}` : p.type))
            .join(", ");
    },
    capitalizeFirstLetter(str) {
        return str.charAt(0).toUpperCase() + str.slice(1);
    },
};
