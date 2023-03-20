type tprise = Bac | Reglette | Pince | Plat;;

type prise = {
  difficulte: float;
  type: tprise;
  angle: int;
};;

let a = [|{difficulte = 5.5; type = Pince; angle = 0}|]