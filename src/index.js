import {Elm} from "./Main.elm";

window.initHexes = function(element) {
  Elm.Main.init({
    node: element
  })
}