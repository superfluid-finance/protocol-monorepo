module.exports = {
  printParams(params) {
    return params.map(p => p.name ? `${p.type} ${p.name}` : p.type).join(', ');
  }
};
