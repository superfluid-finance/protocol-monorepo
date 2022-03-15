import os
import sys
from datetime import datetime

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

if __name__ == '__main__':
    ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
    for filename in os.listdir(ROOT_DIR + "/" + sys.argv[1]):
        if filename.endswith(".csv"):
            print("Trying to generate a chart for: " + filename)
            csv = pd.read_csv(sys.argv[1] + "/" + filename)
            owedDepositChanged = False
            collumns = 2
            for owedDeposit in csv["owedDeposit"]:
                if owedDeposit != 0:
                    owedDepositChanged = True
                    collumns = 3
            fig = make_subplots(rows=len(csv.alias.unique()) + 1, cols=collumns, shared_xaxes=False)
            for num, alias in enumerate(csv.alias.unique()):
                show_legend = True
                if num > 0:
                    show_legend = False
                alias_csv = csv[csv["alias"] == alias]
                timestamp_in_date = []
                for i, timestamp in enumerate(alias_csv["timestamp"]):
                    timestamp_in_date.append(datetime.fromtimestamp(timestamp))
                fig.append_trace(
                    go.Scatter(x=timestamp_in_date, y=alias_csv["availableBalance"], text=alias_csv["description"],
                               fill='tozeroy', name="Available Balance", textposition="top center",
                               legendgroup="balance", showlegend=show_legend, mode="lines+markers+text", legendrank=0,
                               line=dict(color="#1e81b0")), row=num + 1, col=1)
                fig.append_trace(
                    go.Scatter(x=timestamp_in_date, y=alias_csv["availableBalance"],
                               legendgroup="balance", showlegend=False, mode="markers", legendrank=0,
                               line=dict(color="#1e81b0")), row=num + 1, col=1)
                fig.add_annotation(xref="x domain", yref="y domain", x=0.5, y=1.15, showarrow=False,
                                   text="Balances of " + alias_csv["alias"].iloc[0], row=num + 1, col=1)
                fig.append_trace(
                    go.Scatter(x=timestamp_in_date, y=alias_csv["deposit"], name="Deposit", legendgroup="deposit",
                               showlegend=show_legend, mode="lines+markers+text", legendrank=1,
                               textposition="top center", text=alias_csv["deposit"],
                               line=dict(color="#f10303")), row=num + 1, col=2)
                fig.add_annotation(xref="x domain", yref="y domain", x=0.5, y=1.15, showarrow=False,
                                   text="Deposits of " + alias_csv["alias"].iloc[0], row=num + 1, col=2)

                if owedDepositChanged:
                    fig.append_trace(go.Scatter(x=timestamp_in_date, y=alias_csv["owedDeposit"], name="Owed Deposit",
                                                textposition="top center", text=alias_csv["owedDeposit"],
                                                legendgroup="owedDeposit", showlegend=show_legend,
                                                mode="lines+markers+text", legendrank=2, line=dict(color="#37f103")),
                                     row=num + 1, col=3)
                    fig.add_annotation(xref="x domain", yref="y domain", x=0.5, y=1.15, showarrow=False,
                                       text="OwedDeposit of " + alias_csv["alias"].iloc[0], row=num + 1, col=3)
                    fig.update_yaxes(range=[csv["owedDeposit"].min() - 5, csv["owedDeposit"].max() + 5],
                                     row=num + 1, col=3)

                fig.update_yaxes(range=[csv["availableBalance"].min() - 100, csv["availableBalance"].max() + 100],
                                 row=num + 1, col=1)

                fig.update_yaxes(range=[csv["deposit"].min() - 5, csv["deposit"].max() + 5],
                                 row=num + 1, col=2)

                fig.update_xaxes(matches='x' + str(len(csv.alias.unique()) + 1), row=1, col=2)
                fig.update_xaxes(matches='x' + str(len(csv.alias.unique()) + 1), row=1, col=3)
            fig.add_annotation(
                text='<br><b>Events list:</b><br>STR:"alias": Alice starting a stream to "alias"  <br>LIQ: Alice getting liquidated',
                align='left',
                showarrow=False,
                xref='paper',
                yref='paper',
                x=1.23,
                y=0.85)

            unique_timestamps = csv.timestamp.unique()
            total_supply_arr = []
            unique_timestamp_in_date = []
            for timestamp in unique_timestamps:
                unique_timestamp_in_date.append(datetime.fromtimestamp(timestamp))
                timestamp_csv = csv[csv["timestamp"] == timestamp]
                total_supply_arr.append(
                    timestamp_csv["availableBalance"].sum() + timestamp_csv["deposit"].sum() - timestamp_csv[
                        "owedDeposit"].sum())
            fig.append_trace(go.Scatter(x=unique_timestamp_in_date, y=total_supply_arr, fill="tozeroy",
                                        name="Total Supply\n(Available balance + deposit - owedDeposit of all users)",
                                        mode="lines+markers", textposition="top center"),
                             row=len(csv.alias.unique()) + 1, col=1)
            fig.append_trace(
                go.Scatter(x=unique_timestamp_in_date, y=total_supply_arr, showlegend=False, mode="markers"),
                row=len(csv.alias.unique()) + 1, col=1)
            fig.add_annotation(xref="x domain", yref="y domain", x=0.5, y=1.15, showarrow=False,
                               text="Total supply over time", row=len(csv.alias.unique()) + 1, col=1)
            fig.update_yaxes(range=[-10, np.max(total_supply_arr) + 100],
                             row=len(csv.alias.unique()) + 1, col=1)
            if not os.path.exists(ROOT_DIR + "/" + sys.argv[1] + "/" + "images"):
                os.mkdir(ROOT_DIR + "/" + sys.argv[1] + "/" + "images")
            fig.update_layout(title="Test case data from: " + filename)
            fig.write_image(sys.argv[1] + "/" + "images/" + os.path.basename(filename) + ".jpeg", width=1920,
                            height=1080)
