type tprise = Bac | Reglette | Pince | Plat;;

type prise = {
  diff: float;
  p: tprise;
  a: int;
};;

let a = [|{diff = 5.5; p = Pince; a = 0}|]