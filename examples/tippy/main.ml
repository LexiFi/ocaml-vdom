module V = Vdom

let source =
  [
    ( "Bérénice",
      {|
Ah ! cruel ! est-il temps de me le déclarer ?
Qu’avez-vous fait ? Hélas ! je me suis crue aimée.
Au plaisir de vous voir mon âme accoutumée
Ne vit plus que pour vous. Ignoriez-vous vos lois
Quand je vous l’avouai pour la première fois ?
À quel excès d’amour m’avez-vous amenée ?
Que ne me disiez-vous : « Princesse infortunée,
Où vas-tu t’engager, et quel est ton espoir ?
Ne donne point un cœur qu’on ne peut recevoir. »
Ne l’avez-vous reçu, cruel, que pour le rendre,
Quand de vos seules mains ce cœur voudrait dépendre ?
Tout l’empire a vingt fois conspiré contre nous.
Il était temps encor : que ne me quittiez-vous ?
Mille raisons alors consolaient ma misère :
Je pouvais de ma mort accuser votre père,
Le peuple, le sénat, tout l’empire romain,
Tout l’univers, plutôt qu’une si chère main.
Leur haine, dès longtemps contre moi déclarée,
M’avait à mon malheur dès longtemps préparée.
Je n’aurais pas, Seigneur, reçu ce coup cruel
Dans le temps que j’espère un bonheur immortel,
Quand votre heureux amour peut tout ce qu’il désire,
Lorsque Rome se tait, quand votre père expire,
Lorsque tout l’univers fléchit à vos genoux,
Enfin quand je n’ai plus à redouter que vous.
|}
    );
    ( "Titus",
      {|
Et c’est moi seul aussi qui pouvais me détruire.
Je pouvais vivre alors et me laisser séduire ;
Mon cœur se gardait bien d’aller dans l’avenir
Chercher ce qui pouvait un jour nous désunir.
Je voulais qu’à mes vœux rien ne fût invincible,
Je n’examinais rien, j’espérais l’impossible.
Que sais-je ? J’espérais de mourir à vos yeux,
Avant que d’en venir à ces cruels adieux.
Les obstacles semblaient renouveler ma flamme,
Tout l’empire parlait, mais la gloire, Madame,
Ne s’était point encor fait entendre à mon cœur
Du ton dont elle parle au cœur d’un empereur.
Je sais tous les tourments où ce dessein me livre,
Je sens bien que sans vous je ne saurais plus vivre,
Que mon cœur de moi-même est prêt à s’éloigner,
Mais il ne s’agit plus de vivre, il faut régner.
|}
    );
    ( "Bérénice",
      {|
Eh bien ! régnez, cruel, contentez votre gloire :
Je ne dispute plus. J’attendais, pour vous croire,
Que cette même bouche, après mille serments
D’un amour qui devait unir tous nos moments,
Cette bouche, à mes yeux s’avouant infidèle,
M’ordonnât elle-même une absence éternelle.
Moi-même j’ai voulu vous entendre en ce lieu.
Je n’écoute plus rien, et pour jamais : adieu...
Pour jamais ! Ah, Seigneur ! songez-vous en vous-même
Combien ce mot cruel est affreux quand on aime ?
Dans un mois, dans un an, comment souffrirons-nous,
Seigneur, que tant de mers me séparent de vous ?
Que le jour recommence et que le jour finisse,
Sans que jamais Titus puisse voir Bérénice,
Sans que de tout le jour je puisse voir Titus ?
Mais quelle est mon erreur, et que de soins perdus !
L’ingrat, de mon départ consolé par avance,
Daignera-t-il compter les jours de mon absence ?
Ces jours si longs pour moi lui sembleront trop courts.
|}
    );
    ( "Titus",
      {|
Je n’aurai pas, madame, à compter tant de jours :
J’espère que bientôt la triste renommée
Vous fera confesser que vous étiez aimée.
Vous verrez que Titus n’a pu, sans expirer…
|}
    );
    ( "Bérénice",
      {|
Ah, seigneur ! s’il est vrai, pourquoi nous séparer ?
Je ne vous parle point d’un heureux hyménée.
Rome à ne vous plus voir m’a-t-elle condamnée ?
Pourquoi m’enviez-vous l’air que vous respirez ?
|}
    );
    ( "Titus",
      {|
Hélas ! vous pouvez tout, madame : demeurez ;
Je n’y résiste point. Mais je sens ma faiblesse :
Il faudra vous combattre et vous craindre sans cesse,
Et sans cesse veiller à retenir mes pas,
Que vers vous à toute heure entraînent vos appas.
Que dis-je ? en ce moment mon cœur, hors de lui-même,
S’oublie, et se souvient seulement qu’il vous aime.
|}
    );
    ( "Bérénice",
      {|
Eh bien, seigneur, eh bien, qu’en peut-il arriver ?
Voyez-vous les Romains prêts à se soulever ?
|}
    );
    ( "Titus",
      {|
Et qui sait de quel œil ils prendront cette injure ?
S’ils parlent, si les cris succèdent au murmure,
Faudra-t-il par le sang justifier mon choix ?
S’ils se taisent, madame, et me vendent leurs lois,
À quoi m’exposez-vous ? Par quelle complaisance
Faudra-t-il quelque jour payer leur patience ?
Que n’oseront-ils point alors me demander ?
Maintiendrai-je des lois que je ne puis garder ?
|}
    );
    ("Bérénice", {|
Vous ne comptez pour rien les pleurs de Bérénice !
|});
    ("Titus", {|
Je les compte pour rien ! Ah ciel ! quelle injustice !
|});
    ( "Bérénice",
      {|
Quoi ! pour d’injustes lois que vous pouvez changer,
En d’éternels chagrins vous-même vous plonger !
Rome a ses droits, seigneur : n’avez-vous pas les vôtres ?
Ses intérêts sont-ils plus sacrés que les nôtres ?
Dites, parlez.
|}
    );
    ("Titus", {|
Hélas ! que vous me déchirez !
|});
    ("Bérénice", {|
Vous êtes empereur, seigneur, et vous pleurez !
|});
    ( "Titus",
      {|
Oui, madame, il est vrai, je pleure, je soupire,
Je frémis. Mais enfin, quand j’acceptai l’empire,
Rome me fit jurer de maintenir ses droits :
Je dois les maintenir. Déjà, plus d’une fois,
Rome a de mes pareils exercé la constance.
Ah ! si vous remontiez jusques à sa naissance,
Vous les verriez toujours à ses ordres soumis :
L’un, jaloux de sa foi, va chez les ennemis
|}
    );
  ]
  |> List.map (fun (name, txt) ->
      ( name,
        String.split_on_char '\n' txt
        |> List.map String.trim
        |> List.filter (( <> ) "")
        |> List.map (fun line -> String.split_on_char ' ' line) ))

type model = { data: (string * string list list) list; height: int; width: int }

let shuffle l =
  let a = Array.of_list l in
  let n = Array.length a in
  let swap i j =
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  in
  for i = n - 1 downto 1 do
    let j = Random.int i in
    swap i j
  done;
  Array.to_list a

let shuffle_model model =
  shuffle model
  |> List.map (fun (name, lines) -> (name, shuffle lines |> List.map shuffle))

type msg = Shuffle | Clear | Reset | Resize of { width: int; height: int }

let button msg label =
  V.input ~a:[ V.type_button; V.value label; V.onclick (fun _ -> msg) ] []

let span ?a x = V.elt ?a "div" [ x ]

let t = V.text

let br = V.elt "br" []

let p = V.elt "p"

let col ?(a = []) = V.elt "div" ~a:(V.add_class "column" a)

let row ?(a = []) = V.elt "div" ~a:(V.add_class "row" a)

let tooltip txt l =
  V.div
    ~a:(V.add_class "tooltipable" [])
    (Register.Tippy.tooltip ~trigger:[ Click ] txt :: l)

let render_paragraph lines =
  p
    ~a:(V.add_class "scrollable" [])
    (List.map
       (fun words ->
          List.map (fun w -> [ tooltip w [ t w ]; t " " ]) words @ [ [ br ] ]
          |> List.flatten
       )
       lines
     |> List.flatten
    )

let resize =
  Register.Window.onresize (fun _ ->
      let open Js_browser in
      let height = Window.inner_height window |> int_of_float in
      let width = Window.inner_width window |> int_of_float in
      Some (Resize { height; width }))

let view model =
  let container = if model.height > model.width then col else row in
  V.div
    ~a:(V.add_class "scrollable" (V.add_class "root" []))
    (resize
     :: col
       [
         button Shuffle "Shuffle !";
         button Clear "Clear";
         button Reset "Reset";
         t (Printf.sprintf "h: %d w:%d" model.height model.width);
       ]
     :: List.map
       (fun (name, txt) ->
          container [ row [ t name ]; row [ render_paragraph txt ] ]
       )
       model.data
    )

let init = V.return { data = source; height = 1; width = 0 }

let update model = function
  | Shuffle -> V.return { model with data = shuffle_model model.data }
  | Resize { width; height } -> V.return { model with height; width }
  | Reset -> init
  | Clear -> V.return { data = []; height = 1; width = 0 }

let app = V.app ~init ~view ~update ()

open Js_browser

let run () =
  let container = Document.body document in
  ignore (Vdom_blit.run ~container app)

let () = Window.set_onload window run
