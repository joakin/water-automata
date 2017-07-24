import "./main.css";
import * as Elm from "./Main.elm";

const root = document.getElementById("root");
Elm.Main.embed(root, { d: "" });
