export function flowForHumans(flow, cadence = " /month") {
  return (flow * ((3600 * 24 * 30) / 1e18)).toFixed(2) + cadence;
}

export function showTick(bool) {
  if (typeof bool === "undefined") return "";
  if (bool) return "✔️";
}
