import styled from "styled-components";

export const Header = styled.header`
  background-color: #000;
  min-height: 70px;
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: flex-end;
  color: white;
  padding-left: 18px;
`;

export const Body = styled.div`
  align-items: center;
  color: white;
  font-size: 20px;
  justify-content: center;
  background-color: #000;
  padding-bottom: 18px;
`;

export const BoxContainer = styled.div`
  min-height: 60vh;
  background-size: 70vh; /* You must set a specified height */
  background-position-x: center; /* Center the image */
  background-repeat: no-repeat; /* Do not repeat the image */
  ${({ winner }) => {
    switch (winner) {
      case "winner":
        return `background-image: url("./winning.gif");`;
      case "loser":
        return `background-image: url("./waterfall.gif");`;
      default:
        return `background-image: url("./neutral.gif");`;
    }
  }}
  display: flex;
  width: 100%;
  flex-direction: row;
`;

export const Image = styled.img`
  height: 10vmin;
  margin-bottom: 16px;
  pointer-events: none;
`;

export const Link = styled.a.attrs({
  target: "_blank",
  rel: "noopener noreferrer"
})`
  color: #61dafb;
  margin-top: 10px;
`;

export const Div100 = styled.div`
  width: 100%;
`;
export const Button = styled.button`
  background-color: #cd4337;
  border: none;
  border-radius: 8px;
  color: #fff;
  box-shadow: 0px 0px 10px 6px rgba(200, 200, 255, 0.75);
  cursor: pointer;
  font-size: 22px;
  text-align: center;
  text-decoration: none;
  margin: 10px 20px;
  padding: 12px 24px;
  white-space: nowrap;
  :disabled {
    background: #444;
  }

  ${props => props.hidden && "hidden"} :focus {
    border: none;
    outline: none;
  }
`;

export const Center = styled.div`
  text-align: center;
`;
export const Right = styled.div`
  text-align: right;
`;

export const Box = styled.div`
  font-size: 1em;
  max-width: 50%;
  padding: 14px;
  flex-grow: 1;
`;
export const ShrinkBox = styled.div`
  flex-grow: 0;
  display: flex;
  flex-direction: column;
`;

export const Span = styled.span`
  ${({ color }) => `color: ${color}`}
`;

export const BottomTable = styled.div`
  min-height: 40vh;
  background: #000;
  padding: 14px;
  h3 {
    padding-left: 42px;
  }
  td {
    padding: 2px 5px;
  }
`;

export const Left = styled.span`
  text-align: left !important;
`;

export const XL = styled.span`
  font-size: 2em;
`;
