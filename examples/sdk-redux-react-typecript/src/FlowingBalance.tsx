import React, { FC, ReactElement, createRef, useEffect } from "react";
import { Decimal } from "decimal.js";

export const FlowingBalance: FC<{
    balance: string;
    balanceTimestamp: string;
    flowRate: string;
    format?: (flowingBalance: string) => string;
}> = ({ balance, balanceTimestamp, flowRate, format = x => x }): ReactElement => {
    const ref = createRef<HTMLSpanElement>();
    useEffect(() => {
        const balanceDecimal = new Decimal(balance);
        const flowRateDecimal = new Decimal(flowRate).div(1000);
        const balanceTimestampDecimal = new Decimal(balanceTimestamp).mul(1000);

        let stopAnimation = false;
        let lastAnimationTimestamp: DOMHighResTimeStamp = 0;

        const animationStep = (currentAnimationTimestamp: DOMHighResTimeStamp) => {
            if (currentAnimationTimestamp - lastAnimationTimestamp > 100) {
                if (stopAnimation || !ref.current) {
                    return;
                }

                const currentTimestampDecimal = new Decimal(
                    new Date().getTime()
                );

                ref.current.innerHTML = format(balanceDecimal
                    .add(
                        currentTimestampDecimal
                            .sub(balanceTimestampDecimal)
                            .mul(flowRateDecimal)
                    )
                    .toString());

                lastAnimationTimestamp = currentAnimationTimestamp;
            }
            window.requestAnimationFrame(animationStep);
        };

        window.requestAnimationFrame(animationStep);

        return () => {
            stopAnimation = true;
        };
    });
    return <span ref={ref}>{""}</span>;
};
