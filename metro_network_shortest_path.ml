(* ex8.5 | metro *)
(* 駅名を表すレコード型 *)
type station_t = {
  kanji: string; (* 漢字表記 *)
  kana: string; (* かな表記 *)
  roman: string; (* ローマ字表記 *)
  line: string; (* 路線 *)
};;

(* ex8.6 | metro *)
(* stationを受け取って, (路線名, 駅名(かな表示))の文字列を返す *)
let display station = match station with
 { kana = k; line = l } -> l ^ ", " ^ k ;;

let test1 = display { kanji = "漢字"; kana = "かんじ"; roman = "kanji"; line = "ocaml線" } = "ocaml線, かんじ";;

(* ex8.7 | metro *)
type edge_t = {
  src: string; (* 起点の駅名(漢字) *)
  dest: string; (* 終点の駅名(漢字) *)
  via: string; (* 経由する路線名(漢字) *)
  distance: float; (* 2駅間の距離(km) *)
  required_time: int (* 所要時間(分) *)
};;

(* ex9.9 | metro *)
let global_ekimei_list = [
  {kanji="代々木上原"; kana="よよぎうえはら"; roman="yoyogiuehara"; line="千代田線"};
  {kanji="代々木公園"; kana="よよぎこうえん"; roman="yoyogikouen"; line="千代田線"};
  {kanji="明治神宮前"; kana="めいじじんぐうまえ"; roman="meijijinguumae"; line="千代田線"};
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
  {kanji="霞ヶ関"; kana="かすみがせき"; roman="kasumigaseki"; line="千代田線"};
  {kanji="日比谷"; kana="ひびや"; roman="hibiya"; line="千代田線"};
  {kanji="二重橋前"; kana="にじゅうばしまえ"; roman="nijuubasimae"; line="千代田線"};
  {kanji="大手町"; kana="おおてまち"; roman="otemachi"; line="千代田線"};
  {kanji="新御茶ノ水"; kana="しんおちゃのみず"; roman="shin-ochanomizu"; line="千代田線"};
  {kanji="湯島"; kana="ゆしま"; roman="yushima"; line="千代田線"};
  {kanji="根津"; kana="ねづ"; roman="nedu"; line="千代田線"};
  {kanji="千駄木"; kana="せんだぎ"; roman="sendagi"; line="千代田線"};
  {kanji="西日暮里"; kana="にしにっぽり"; roman="nishinippori"; line="千代田線"};
  {kanji="町屋"; kana="まちや"; roman="machiya"; line="千代田線"};
  {kanji="北千住"; kana="きたせんじゅ"; roman="kitasenjyu"; line="千代田線"};
  {kanji="綾瀬"; kana="あやせ"; roman="ayase"; line="千代田線"};
  {kanji="北綾瀬"; kana="きたあやせ"; roman="kitaayase"; line="千代田線"};
  {kanji="浅草"; kana="あさくさ"; roman="asakusa"; line="銀座線"};
  {kanji="田原町"; kana="たわらまち"; roman="tawaramachi"; line="銀座線"};
  {kanji="稲荷町"; kana="いなりちょう"; roman="inaricho"; line="銀座線"};
  {kanji="上野"; kana="うえの"; roman="ueno"; line="銀座線"};
  {kanji="上野広小路"; kana="うえのひろこうじ"; roman="uenohirokoji"; line="銀座線"};
  {kanji="末広町"; kana="すえひろちょう"; roman="suehirocho"; line="銀座線"};
  {kanji="神田"; kana="かんだ"; roman="kanda"; line="銀座線"};
  {kanji="三越前"; kana="みつこしまえ"; roman="mitsukoshimae"; line="銀座線"};
  {kanji="日本橋"; kana="にほんばし"; roman="nihonbashi"; line="銀座線"};
  {kanji="京橋"; kana="きょうばし"; roman="kyobashi"; line="銀座線"};
  {kanji="銀座"; kana="ぎんざ"; roman="ginza"; line="銀座線"};
  {kanji="新橋"; kana="しんばし"; roman="shinbashi"; line="銀座線"};
  {kanji="虎ノ門"; kana="とらのもん"; roman="toranomon"; line="銀座線"};
  {kanji="溜池山王"; kana="ためいけさんのう"; roman="tameikesannou"; line="銀座線"};
  {kanji="赤坂見附"; kana="あかさかみつけ"; roman="akasakamitsuke"; line="銀座線"};
  {kanji="青山一丁目"; kana="あおやまいっちょうめ"; roman="aoyamaicchome"; line="銀座線"};
  {kanji="外苑前"; kana="がいえんまえ"; roman="gaienmae"; line="銀座線"};
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesando"; line="銀座線"};
  {kanji="渋谷"; kana="しぶや"; roman="shibuya"; line="銀座線"};
  {kanji="渋谷"; kana="しぶや"; roman="shibuya"; line="半蔵門線"};
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="半蔵門線"};
  {kanji="青山一丁目"; kana="あおやまいっちょうめ"; roman="aoyama-itchome"; line="半蔵門線"};
  {kanji="永田町"; kana="ながたちょう"; roman="nagatacho"; line="半蔵門線"};
  {kanji="半蔵門"; kana="はんぞうもん"; roman="hanzomon"; line="半蔵門線"};
  {kanji="九段下"; kana="くだんした"; roman="kudanshita"; line="半蔵門線"};
  {kanji="神保町"; kana="じんぼうちょう"; roman="jinbocho"; line="半蔵門線"};
  {kanji="大手町"; kana="おおてまち"; roman="otemachi"; line="半蔵門線"};
  {kanji="三越前"; kana="みつこしまえ"; roman="mitsukoshimae"; line="半蔵門線"};
  {kanji="水天宮前"; kana="すいてんぐうまえ"; roman="suitengumae"; line="半蔵門線"};
  {kanji="清澄白河"; kana="きよすみしらかわ"; roman="kiyosumi-shirakawa"; line="半蔵門線"};
  {kanji="住吉"; kana="すみよし"; roman="sumiyoshi"; line="半蔵門線"};
  {kanji="錦糸町"; kana="きんしちょう"; roman="kinshicho"; line="半蔵門線"};
  {kanji="押上"; kana="おしあげ"; roman="oshiage"; line="半蔵門線"};
  {kanji="中目黒"; kana="なかめぐろ"; roman="nakameguro"; line="日比谷線"};
  {kanji="恵比寿"; kana="えびす"; roman="ebisu"; line="日比谷線"};
  {kanji="広尾"; kana="ひろお"; roman="hiro"; line="日比谷線"};
  {kanji="六本木"; kana="ろっぽんぎ"; roman="roppongi"; line="日比谷線"};
  {kanji="神谷町"; kana="かみやちょう"; roman="kamiyacho"; line="日比谷線"};
  {kanji="霞ヶ関"; kana="かすみがせき"; roman="kasumigaseki"; line="日比谷線"};
  {kanji="日比谷"; kana="ひびや"; roman="hibiya"; line="日比谷線"};
  {kanji="銀座"; kana="ぎんざ"; roman="ginza"; line="日比谷線"};
  {kanji="東銀座"; kana="ひがしぎんざ"; roman="higashiginza"; line="日比谷線"};
  {kanji="築地"; kana="つきじ"; roman="tsukiji"; line="日比谷線"};
  {kanji="八丁堀"; kana="はっちょうぼり"; roman="hacchobori"; line="日比谷線"};
  {kanji="茅場町"; kana="かやばちょう"; roman="kayabacho"; line="日比谷線"};
  {kanji="人形町"; kana="にんぎょうちょう"; roman="ningyomachi"; line="日比谷線"};
  {kanji="小伝馬町"; kana="こでんまちょう"; roman="kodemmacho"; line="日比谷線"};
  {kanji="秋葉原"; kana="あきはばら"; roman="akihabara"; line="日比谷線"};
  {kanji="仲御徒町"; kana="なかおかちまち"; roman="nakaokachimachi"; line="日比谷線"};
  {kanji="上野"; kana="うえの"; roman="ueno"; line="日比谷線"};
  {kanji="入谷"; kana="いりや"; roman="iriya"; line="日比谷線"};
  {kanji="三ノ輪"; kana="みのわ"; roman="minowa"; line="日比谷線"};
  {kanji="南千住"; kana="みなみせんじゅ"; roman="minamisenju"; line="日比谷線"};
  {kanji="北千住"; kana="きたせんじゅ"; roman="kitasenju"; line="日比谷線"};
  {kanji="池袋"; kana="いけぶくろ"; roman="ikebukuro"; line="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; roman="shinotsuka"; line="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; roman="myogadani"; line="丸ノ内線"};
  {kanji="後楽園"; kana="こうらくえん"; roman="korakuen"; line="丸ノ内線"};
  {kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; roman="hongosanchome"; line="丸ノ内線"};
  {kanji="御茶ノ水"; kana="おちゃのみず"; roman="ochanomizu"; line="丸ノ内線"};
  {kanji="淡路町"; kana="あわじちょう"; roman="awajicho"; line="丸ノ内線"};
  {kanji="大手町"; kana="おおてまち"; roman="otemachi"; line="丸ノ内線"};
  {kanji="東京"; kana="とうきょう"; roman="tokyo"; line="丸ノ内線"};
  {kanji="銀座"; kana="ぎんざ"; roman="ginza"; line="丸ノ内線"};
  {kanji="霞ヶ関"; kana="かすみがせき"; roman="kasumigaseki"; line="丸ノ内線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidomae"; line="丸ノ内線"};
  {kanji="赤坂見附"; kana="あかさかみつけ"; roman="akasakamitsuke"; line="丸ノ内線"};
  {kanji="四ツ谷"; kana="よつや"; roman="yotsuya"; line="丸ノ内線"};
  {kanji="四谷三丁目"; kana="よつやさんちょうめ"; roman="yotsuyasanchome"; line="丸ノ内線"};
  {kanji="新宿御苑前"; kana="しんじゅくぎょえんまえ"; roman="shinjuku-gyoemmae"; line="丸ノ内線"};
  {kanji="新宿三丁目"; kana="しんじゅくさんちょうめ"; roman="shinjuku-sanchome"; line="丸ノ内線"};
  {kanji="新宿"; kana="しんじゅく"; roman="shinjuku"; line="丸ノ内線"};
  {kanji="西新宿"; kana="にししんじゅく"; roman="nishi-shinjuku"; line="丸ノ内線"};
  {kanji="中野坂上"; kana="なかのさかうえ"; roman="nakano-sakaue"; line="丸ノ内線"};
  {kanji="新中野"; kana="しんなかの"; roman="shin-nakano"; line="丸ノ内線"};
  {kanji="東高円寺"; kana="ひがしこうえんじ"; roman="higashi-koenji"; line="丸ノ内線"};
  {kanji="新高円寺"; kana="しんこうえんじ"; roman="shin-koenji"; line="丸ノ内線"};
  {kanji="南阿佐ヶ谷"; kana="みなみあさがや"; roman="minami-asagaya"; line="丸ノ内線"};
  {kanji="荻窪"; kana="おぎくぼ"; roman="ogikubo"; line="丸ノ内線"};
  {kanji="中野新橋"; kana="なかのしんばし"; roman="nakano-shimbashi"; line="丸ノ内線"};
  {kanji="中野富士見町"; kana="なかのふじみちょう"; roman="nakano-fujimicho"; line="丸ノ内線"};
  {kanji="方南町"; kana="ほうなんちょう"; roman="honancho"; line="丸ノ内線"};
  {kanji="四ツ谷"; kana="よつや"; roman="yotsuya"; line="南北線"};
  {kanji="永田町"; kana="ながたちょう"; roman="nagatacho"; line="南北線"};
  {kanji="溜池山王"; kana="ためいけさんのう"; roman="tameikesanno"; line="南北線"};
  {kanji="六本木一丁目"; kana="ろっぽんぎいっちょうめ"; roman="roppongiitchome"; line="南北線"};
  {kanji="麻布十番"; kana="あざぶじゅうばん"; roman="azabujuban"; line="南北線"};
  {kanji="白金高輪"; kana="しろかねたかなわ"; roman="shirokanetakanawa"; line="南北線"};
  {kanji="白金台"; kana="しろかねだい"; roman="shirokanedai"; line="南北線"};
  {kanji="目黒"; kana="めぐろ"; roman="meguro"; line="南北線"};
  {kanji="市ヶ谷"; kana="いちがや"; roman="ichigaya"; line="南北線"};
  {kanji="飯田橋"; kana="いいだばし"; roman="idabashi"; line="南北線"};
  {kanji="後楽園"; kana="こうらくえん"; roman="korakuen"; line="南北線"};
  {kanji="東大前"; kana="とうだいまえ"; roman="todaimae"; line="南北線"};
  {kanji="本駒込"; kana="ほんこまごめ"; roman="honkomagome"; line="南北線"};
  {kanji="駒込"; kana="こまごめ"; roman="komagome"; line="南北線"};
  {kanji="西ヶ原"; kana="にしがはら"; roman="nishigahara"; line="南北線"};
  {kanji="王子"; kana="おうじ"; roman="oji"; line="南北線"};
  {kanji="王子神谷"; kana="おうじかみや"; roman="ojikamiya"; line="南北線"};
  {kanji="志茂"; kana="しも"; roman="shimo"; line="南北線"};
  {kanji="赤羽岩淵"; kana="あかばねいわぶち"; roman="akabaneiwabuchi"; line="南北線"};
  {kanji="西船橋"; kana="にしふなばし"; roman="nishi-funabashi"; line="東西線"};
  {kanji="原木中山"; kana="ばらきなかやま"; roman="baraki-nakayama"; line="東西線"};
  {kanji="妙典"; kana="みょうでん"; roman="myoden"; line="東西線"};
  {kanji="行徳"; kana="ぎょうとく"; roman="gyotoku"; line="東西線"};
  {kanji="南行徳"; kana="みなみぎょうとく"; roman="minami-gyotoku"; line="東西線"};
  {kanji="浦安"; kana="うらやす"; roman="urayasu"; line="東西線"};
  {kanji="葛西"; kana="かさい"; roman="kasai"; line="東西線"};
  {kanji="西葛西"; kana="にしかさい"; roman="nishi-kasai"; line="東西線"};
  {kanji="南砂町"; kana="みなみすなまち"; roman="minami-sunamachi"; line="東西線"};
  {kanji="東陽町"; kana="とうようちょう"; roman="touyoucho"; line="東西線"};
  {kanji="木場"; kana="きば"; roman="kiba"; line="東西線"};
  {kanji="門前仲町"; kana="もんぜんなかちょう"; roman="monzen-nakacho"; line="東西線"};
  {kanji="茅場町"; kana="かやばちょう"; roman="kayabacho"; line="東西線"};
  {kanji="日本橋"; kana="にほんばし"; roman="nihonbashi"; line="東西線"};
  {kanji="大手町"; kana="おおてまち"; roman="otemachi"; line="東西線"};
  {kanji="竹橋"; kana="たけばし"; roman="takebashi"; line="東西線"};
  {kanji="九段下"; kana="くだんした"; roman="kudanshita"; line="東西線"};
  {kanji="飯田橋"; kana="いいだばし"; roman="iidabashi"; line="東西線"};
  {kanji="神楽坂"; kana="かぐらざか"; roman="kagurazaka"; line="東西線"};
  {kanji="早稲田"; kana="わせだ"; roman="waseda"; line="東西線"};
  {kanji="高田馬場"; kana="たかだのばば"; roman="takadanobaba"; line="東西線"};
  {kanji="落合"; kana="おちあい"; roman="ochiai"; line="東西線"};
  {kanji="中野"; kana="なかの"; roman="nakano"; line="東西線"};
  {roman="shinkiba"; kana="しんきば"; kanji="新木場"; line="有楽町線"};
  {roman="tatsumi"; kana="たつみ"; kanji="辰巳"; line="有楽町線"};
  {roman="toyosu"; kana="とよす"; kanji="豊洲"; line="有楽町線"};
  {roman="tsukishima"; kana="つきしま"; kanji="月島"; line="有楽町線"};
  {roman="shintomityou"; kana="しんとみちょう"; kanji="新富町"; line="有楽町線"};
  {roman="ginzaittyoume"; kana="ぎんざいっちょうめ"; kanji="銀座一丁目"; line="有楽町線"};
  {roman="yuurakutyou"; kana="ゆうらくちょう"; kanji="有楽町"; line="有楽町線"};
  {roman="sakuradamon"; kana="さくらだもん"; kanji="桜田門"; line="有楽町線"};
  {roman="nagatacho"; kana="ながたちょう"; kanji="永田町"; line="有楽町線"};
  {roman="koujimachi"; kana="こうじまち"; kanji="麹町"; line="有楽町線"};
  {roman="ichigaya"; kana="いちがや"; kanji="市ヶ谷"; line="有楽町線"};
  {roman="iidabashi"; kana="いいだばし"; kanji="飯田橋"; line="有楽町線"};
  {kanji="江戸川橋"; kana="えどがわばし"; roman="edogawabasi"; line="有楽町線"};
  {kanji="護国寺"; kana="ごこくじ"; roman="gokokuji"; line="有楽町線"};
  {kanji="東池袋"; kana="ひがしいけぶくろ"; roman="higasiikebukuro"; line="有楽町線"};
  {kanji="池袋"; kana="いけぶくろ"; roman="ikebukuro"; line="有楽町線"};
  {kanji="要町"; kana="かなめちょう"; roman="kanametyou"; line="有楽町線"};
  {kanji="千川"; kana="せんかわ"; roman="senkawa"; line="有楽町線"};
  {kanji="小竹向原"; kana="こたけむかいはら"; roman="kotakemukaihara"; line="有楽町線"};
  {kanji="氷川台"; kana="ひかわだい"; roman="hikawadai"; line="有楽町線"};
  {kanji="平和台"; kana="へいわだい"; roman="heiwadai"; line="有楽町線"};
  {kanji="営団赤塚"; kana="えいだんあかつか"; roman="eidanakakuka"; line="有楽町線"};
  {kanji="営団成増"; kana="えいだんなります"; roman="eidannarimasu"; line="有楽町線"};
  {kanji="和光市"; kana="わこうし"; roman="wakousi"; line="有楽町線"};
]

(* ex9.9 | metro *)
let global_ekikan_list = [
  {src="代々木上原"; dest="代々木公園"; via="千代田線"; distance=1.0; required_time=2};
  {src="代々木公園"; dest="明治神宮前"; via="千代田線"; distance=1.2; required_time=2};
  {src="明治神宮前"; dest="表参道"; via="千代田線"; distance=0.9; required_time=2};
  {src="表参道"; dest="乃木坂"; via="千代田線"; distance=1.4; required_time=3};
  {src="乃木坂"; dest="赤坂"; via="千代田線"; distance=1.1; required_time=2};
  {src="赤坂"; dest="国会議事堂前"; via="千代田線"; distance=0.8; required_time=1};
  {src="国会議事堂前"; dest="霞ヶ関"; via="千代田線"; distance=0.7; required_time=1};
  {src="霞ヶ関"; dest="日比谷"; via="千代田線"; distance=1.2; required_time=2};
  {src="日比谷"; dest="二重橋前"; via="千代田線"; distance=0.7; required_time=1};
  {src="二重橋前"; dest="大手町"; via="千代田線"; distance=0.7; required_time=1};
  {src="大手町"; dest="新御茶ノ水"; via="千代田線"; distance=1.3; required_time=2};
  {src="新御茶ノ水"; dest="湯島"; via="千代田線"; distance=1.2; required_time=2};
  {src="湯島"; dest="根津"; via="千代田線"; distance=1.2; required_time=2};
  {src="根津"; dest="千駄木"; via="千代田線"; distance=1.0; required_time=2};
  {src="千駄木"; dest="西日暮里"; via="千代田線"; distance=0.9; required_time=1};
  {src="西日暮里"; dest="町屋"; via="千代田線"; distance=1.7; required_time=2};
  {src="町屋"; dest="北千住"; via="千代田線"; distance=2.6; required_time=3};
  {src="北千住"; dest="綾瀬"; via="千代田線"; distance=2.5; required_time=3};
  {src="綾瀬"; dest="北綾瀬"; via="千代田線"; distance=2.1; required_time=4};
  {src="浅草"; dest="田原町"; via="銀座線"; distance=0.8; required_time=2};
  {src="田原町"; dest="稲荷町"; via="銀座線"; distance=0.7; required_time=1};
  {src="稲荷町"; dest="上野"; via="銀座線"; distance=0.7; required_time=2};
  {src="上野"; dest="上野広小路"; via="銀座線"; distance=0.5; required_time=2};
  {src="上野広小路"; dest="末広町"; via="銀座線"; distance=0.6; required_time=1};
  {src="末広町"; dest="神田"; via="銀座線"; distance=1.1; required_time=2};
  {src="神田"; dest="三越前"; via="銀座線"; distance=0.7; required_time=1};
  {src="三越前"; dest="日本橋"; via="銀座線"; distance=0.6; required_time=2};
  {src="日本橋"; dest="京橋"; via="銀座線"; distance=0.7; required_time=2};
  {src="京橋"; dest="銀座"; via="銀座線"; distance=0.7; required_time=1};
  {src="銀座"; dest="新橋"; via="銀座線"; distance=0.9; required_time=2};
  {src="新橋"; dest="虎ノ門"; via="銀座線"; distance=0.8; required_time=2};
  {src="虎ノ門"; dest="溜池山王"; via="銀座線"; distance=0.6; required_time=2};
  {src="溜池山王"; dest="赤坂見附"; via="銀座線"; distance=0.9; required_time=2};
  {src="赤坂見附"; dest="青山一丁目"; via="銀座線"; distance=1.3; required_time=2};
  {src="青山一丁目"; dest="外苑前"; via="銀座線"; distance=0.7; required_time=2};
  {src="外苑前"; dest="表参道"; via="銀座線"; distance=0.7; required_time=1};
  {src="表参道"; dest="渋谷"; via="銀座線"; distance=1.3; required_time=1};
  {src="渋谷"; dest="表参道"; via="半蔵門線"; distance=1.3; required_time=2};
  {src="表参道"; dest="青山一丁目"; via="半蔵門線"; distance=1.4; required_time=2};
  {src="青山一丁目"; dest="永田町"; via="半蔵門線"; distance=1.3; required_time=2};
  {src="永田町"; dest="半蔵門"; via="半蔵門線"; distance=1.0; required_time=2};
  {src="半蔵門"; dest="九段下"; via="半蔵門線"; distance=1.6; required_time=2};
  {src="九段下"; dest="神保町"; via="半蔵門線"; distance=0.4; required_time=1};
  {src="神保町"; dest="大手町"; via="半蔵門線"; distance=1.7; required_time=3};
  {src="大手町"; dest="三越前"; via="半蔵門線"; distance=0.7; required_time=1};
  {src="三越前"; dest="水天宮前"; via="半蔵門線"; distance=1.3; required_time=2};
  {src="水天宮前"; dest="清澄白河"; via="半蔵門線"; distance=1.7; required_time=3};
  {src="清澄白河"; dest="住吉"; via="半蔵門線"; distance=1.9; required_time=3};
  {src="住吉"; dest="錦糸町"; via="半蔵門線"; distance=1.; required_time=2};
  {src="錦糸町"; dest="押上"; via="半蔵門線"; distance=1.4; required_time=2};
  {src="中目黒"; dest="恵比寿"; via="日比谷線"; distance=1.; required_time=2};
  {src="恵比寿"; dest="広尾"; via="日比谷線"; distance=1.5; required_time=3};
  {src="広尾"; dest="六本木"; via="日比谷線"; distance=1.7; required_time=3};
  {src="六本木"; dest="神谷町"; via="日比谷線"; distance=1.5; required_time=3};
  {src="神谷町"; dest="霞ヶ関"; via="日比谷線"; distance=1.3; required_time=2};
  {src="霞ヶ関"; dest="日比谷"; via="日比谷線"; distance=1.2; required_time=2};
  {src="日比谷"; dest="銀座"; via="日比谷線"; distance=0.4; required_time=1};
  {src="銀座"; dest="東銀座"; via="日比谷線"; distance=0.4; required_time=1};
  {src="東銀座"; dest="築地"; via="日比谷線"; distance=0.6; required_time=2};
  {src="築地"; dest="八丁堀"; via="日比谷線"; distance=1.; required_time=2};
  {src="八丁堀"; dest="茅場町"; via="日比谷線"; distance=0.5; required_time=1};
  {src="茅場町"; dest="人形町"; via="日比谷線"; distance=0.9; required_time=2};
  {src="人形町"; dest="小伝馬町"; via="日比谷線"; distance=0.6; required_time=1};
  {src="小伝馬町"; dest="秋葉原"; via="日比谷線"; distance=0.9; required_time=2};
  {src="秋葉原"; dest="仲御徒町"; via="日比谷線"; distance=1.; required_time=1};
  {src="仲御徒町"; dest="上野"; via="日比谷線"; distance=0.5; required_time=1};
  {src="上野"; dest="入谷"; via="日比谷線"; distance=1.2; required_time=2};
  {src="入谷"; dest="三ノ輪"; via="日比谷線"; distance=1.2; required_time=2};
  {src="三ノ輪"; dest="南千住"; via="日比谷線"; distance=0.8; required_time=2};
  {src="南千住"; dest="北千住"; via="日比谷線"; distance=1.8; required_time=3};
  {src="池袋"; dest="新大塚"; via="丸ノ内線"; distance=1.8; required_time=3};
  {src="新大塚"; dest="茗荷谷"; via="丸ノ内線"; distance=1.2; required_time=2};
  {src="茗荷谷"; dest="後楽園"; via="丸ノ内線"; distance=1.8; required_time=2};
  {src="後楽園"; dest="本郷三丁目"; via="丸ノ内線"; distance=0.8; required_time=1};
  {src="本郷三丁目"; dest="御茶ノ水"; via="丸ノ内線"; distance=0.8; required_time=1};
  {src="御茶ノ水"; dest="淡路町"; via="丸ノ内線"; distance=0.8; required_time=1};
  {src="淡路町"; dest="大手町"; via="丸ノ内線"; distance=0.9; required_time=2};
  {src="大手町"; dest="東京"; via="丸ノ内線"; distance=0.6; required_time=1};
  {src="東京"; dest="銀座"; via="丸ノ内線"; distance=1.1; required_time=2};
  {src="銀座"; dest="霞ヶ関"; via="丸ノ内線"; distance=1.0; required_time=2};
  {src="霞ヶ関"; dest="国会議事堂前"; via="丸ノ内線"; distance=0.7; required_time=1};
  {src="国会議事堂前"; dest="赤坂見附"; via="丸ノ内線"; distance=0.9; required_time=2};
  {src="赤坂見附"; dest="四ツ谷"; via="丸ノ内線"; distance=1.3; required_time=2};
  {src="四ツ谷"; dest="四谷三丁目"; via="丸ノ内線"; distance=1.0; required_time=2};
  {src="四谷三丁目"; dest="新宿御苑前"; via="丸ノ内線"; distance=0.9; required_time=1};
  {src="新宿御苑前"; dest="新宿三丁目"; via="丸ノ内線"; distance=0.7; required_time=1};
  {src="新宿三丁目"; dest="新宿"; via="丸ノ内線"; distance=0.3; required_time=1};
  {src="新宿"; dest="西新宿"; via="丸ノ内線"; distance=0.8; required_time=1};
  {src="西新宿"; dest="中野坂上"; via="丸ノ内線"; distance=1.1; required_time=2};
  {src="中野坂上"; dest="新中野"; via="丸ノ内線"; distance=1.1; required_time=2};
  {src="新中野"; dest="東高円寺"; via="丸ノ内線"; distance=1.0; required_time=1};
  {src="東高円寺"; dest="新高円寺"; via="丸ノ内線"; distance=0.9; required_time=1};
  {src="新高円寺"; dest="南阿佐ヶ谷"; via="丸ノ内線"; distance=1.2; required_time=2};
  {src="南阿佐ヶ谷"; dest="荻窪"; via="丸ノ内線"; distance=1.5; required_time=2};
  {src="中野坂上"; dest="中野新橋"; via="丸ノ内線"; distance=1.3; required_time=2};
  {src="中野新橋"; dest="中野富士見町"; via="丸ノ内線"; distance=0.6; required_time=1};
  {src="中野富士見町"; dest="方南町"; via="丸ノ内線"; distance=1.3; required_time=2};
  {src="市ヶ谷"; dest="四ツ谷"; via="南北線"; distance=1.0; required_time=2};
  {src="四ツ谷"; dest="永田町"; via="南北線"; distance=1.3; required_time=3};
  {src="永田町"; dest="溜池山王"; via="南北線"; distance=0.9; required_time=1};
  {src="溜池山王"; dest="六本木一丁目"; via="南北線"; distance=0.9; required_time=2};
  {src="六本木一丁目"; dest="麻布十番"; via="南北線"; distance=1.2; required_time=2};
  {src="麻布十番"; dest="白金高輪"; via="南北線"; distance=1.3; required_time=2};
  {src="白金高輪"; dest="白金台"; via="南北線"; distance=1.0; required_time=2};
  {src="白金台"; dest="目黒"; via="南北線"; distance=1.3; required_time=2};
  {src="市ヶ谷"; dest="飯田橋"; via="南北線"; distance=1.1 ; required_time=2};
  {src="飯田橋"; dest="後楽園"; via="南北線"; distance=1.4 ; required_time=2};
  {src="後楽園"; dest="東大前"; via="南北線"; distance=1.3 ; required_time=3};
  {src="東大前"; dest="本駒込"; via="南北線"; distance=0.9 ; required_time=2};
  {src="本駒込"; dest="駒込"; via="南北線"; distance=1.4; required_time=2};
  {src="駒込"; dest="西ヶ原"; via="南北線"; distance=1.4; required_time=2};
  {src="西ヶ原"; dest="王子"; via="南北線"; distance=1.0; required_time=2};
  {src="王子"; dest="王子神谷"; via="南北線"; distance=1.2; required_time=2};
  {src="王子神谷"; dest="志茂"; via="南北線"; distance=1.6; required_time=3};
  {src="志茂"; dest="赤羽岩淵"; via="南北線"; distance=1.1; required_time=2};
  {src="西船橋" ; dest="原木中山"; via="東西線"; distance=1.9; required_time=3};
  {src="原木中山"; dest="妙典"; via="東西線"; distance=2.1 ; required_time=2};
  {src="妙典"; dest="行徳"; via="東西線"; distance=1.3 ; required_time=2};
  {src="行徳"; dest="南行徳"; via="東西線"; distance=1.5 ; required_time=2};
  {src="南行徳"; dest="浦安" ; via="東西線"; distance=1.2 ; required_time=2};
  {src="浦安" ; dest="葛西"; via="東西線"; distance=1.9 ; required_time=2};
  {src="葛西"; dest="西葛西"; via="東西線"; distance=1.2 ; required_time=2};
  {src="西葛西"; dest="南砂町"; via="東西線"; distance=2.7 ; required_time=2};
  {src="南砂町"; dest="東陽町"; via="東西線"; distance=1.2 ; required_time=2};
  {src="東陽町"; dest="木場" ; via="東西線"; distance=0.9 ; required_time=1};
  {src="木場"; dest="門前仲町"; via="東西線"; distance=1.1 ; required_time=1};
  {src="門前仲町"; dest="茅場町"; via="東西線"; distance=1.8 ; required_time=2};
  {src="茅場町"; dest="日本橋"; via="東西線"; distance=0.5 ; required_time=1};
  {src="日本橋"; dest="大手町"; via="東西線"; distance=0.8 ; required_time=1};
  {src="大手町"; dest="竹橋"; via="東西線"; distance=1.0; required_time=2};
  {src="竹橋"; dest="九段下"; via="東西線"; distance=1.0; required_time=1};
  {src="九段下"; dest="飯田橋"; via="東西線"; distance=0.7; required_time=1};
  {src="飯田橋"; dest="神楽坂"; via="東西線"; distance=1.2; required_time=2};
  {src="神楽坂"; dest="早稲田"; via="東西線"; distance=1.2; required_time=2};
  {src="早稲田"; dest="高田馬場"; via="東西線"; distance=1.7; required_time=3};
  {src="高田馬場"; dest="落合"; via="東西線"; distance=1.9; required_time=3};
  {src="落合"; dest="中野"; via="東西線"; distance=2.0; required_time=3};
  {src="新木場"; dest="辰巳"; via="有楽町線"; distance=1.5; required_time=2};
  {src="辰巳"; dest="豊洲"; via="有楽町線"; distance=1.7; required_time=2};
  {src="豊洲"; dest="月島"; via="有楽町線"; distance=1.4; required_time=2};
  {src="月島"; dest="新富町"; via="有楽町線"; distance=1.3; required_time=2};
  {src="新富町"; dest="銀座一丁目"; via="有楽町線"; distance=0.7; required_time=1};
  {src="銀座一丁目"; dest="有楽町"; via="有楽町線"; distance=0.5; required_time=1};
  {src="有楽町"; dest="桜田門"; via="有楽町線"; distance=1.0; required_time=1};
  {src="桜田門"; dest="永田町"; via="有楽町線"; distance=0.9; required_time=2};
  {src="永田町"; dest="麹町"; via="有楽町線"; distance=0.9; required_time=1};
  {src="麹町"; dest="市ヶ谷"; via="有楽町線"; distance=0.9; required_time=1};
  {src="市ヶ谷"; dest="飯田橋"; via="有楽町線"; distance=1.1; required_time=2};
  {src="飯田橋"; dest="江戸川橋"; via="有楽町線"; distance=1.6; required_time=3};
  {src="江戸川橋"; dest="護国寺"; via="有楽町線"; distance=1.3; required_time=2};
  {src="護国寺"; dest="東池袋"; via="有楽町線"; distance=1.1; required_time=2};
  {src="東池袋"; dest="池袋"; via="有楽町線"; distance=2.0; required_time=2};
  {src="池袋"; dest="要町"; via="有楽町線"; distance=1.2; required_time=2};
  {src="要町"; dest="千川"; via="有楽町線"; distance=1.0; required_time=1};
  {src="千川"; dest="小竹向原"; via="有楽町線"; distance=1.0; required_time=2};
  {src="小竹向原"; dest="氷川台"; via="有楽町線"; distance=1.5; required_time=2};
  {src="氷川台"; dest="平和台"; via="有楽町線"; distance=1.4; required_time=2};
  {src="平和台"; dest="営団赤塚"; via="有楽町線"; distance=1.8; required_time=2};
  {src="営団赤塚"; dest="営団成増"; via="有楽町線"; distance=1.5; required_time=2};
  {src="営団成増"; dest="和光市"; via="有楽町線"; distance=2.1; required_time=3};
]

(* ex10.10 *)
(* ローマ字の駅名(string)と駅名リスト(station_t list)を受け取って,その駅の漢字を表記を文字列で返す *)
(* 駅名リストに名前がない場合は空文字を返す *)
(* roman_to_kanji: string -> station_t list -> string *)
let rec roman_to_kanji station_roman_name station_list = match station_list with
    [] -> ""
  | {kanji = kanji; kana = kana; roman = r; line = l} :: rest ->
      if station_roman_name = r
      then kanji
      else roman_to_kanji station_roman_name rest

let test1 = roman_to_kanji "unknown" [] = "";;
let test2 = roman_to_kanji "unknown" global_ekimei_list = "";;
let test3 = roman_to_kanji "yoyogiuehara" global_ekimei_list = "代々木上原";;
let test4 = roman_to_kanji "wakousi" global_ekimei_list = "和光市";;

(* ex10.11 *)
(* 漢字の駅名(string)を２つと駅間リスト(edge_t list)を受け取り、2駅間の距離を返す *)
(* 直接つながっていない場合はinfinityを返す *)
(* get_distance: string -> string -> edge_t list -> float *)
let rec get_distance station_kanji1 station_kanji2 edges = match edges with
    [] -> infinity
  | {src = s; dest = dest; via = v; distance = dis; required_time = rt} :: rest ->
      if s = station_kanji1 && dest = station_kanji2 then dis
      else if s = station_kanji2 && dest = station_kanji1 then dis
      else get_distance station_kanji1 station_kanji2 rest;;

let test1 = get_distance "aa" "bb" [] = infinity;;
let test2 = get_distance "aa" "bb" global_ekikan_list = infinity;;
let test3 = get_distance "代々木上原" "代々木公園" global_ekikan_list = 1.0;;
let test4 = get_distance "代々木公園" "代々木上原" global_ekikan_list = 1.0;;
let test5 = get_distance "営団成増" "和光市" global_ekikan_list = 2.1;;

(* ex10.12 *)
(* ローマ字の２つの駅名(string)を受けとって直接つながっている場合は
   「AからBまでは○kmです」という文字列を返し、つながっていない場合は
   「AとBはつながっていません」 という文字列を返す
*)
(* display_distance: string -> string -> string *)
let display_distance station1 station2 =
  let kanji1 = roman_to_kanji station1 global_ekimei_list in
    if kanji1 = "" then
      station1 ^ "という駅は存在しません"
    else
      let kanji2 = roman_to_kanji station2 global_ekimei_list in
      if kanji2 = "" then
        station2 ^ "という駅は存在しません"
      else
        let distance = get_distance kanji1 kanji2 global_ekikan_list in
          if distance = infinity
          then
            kanji1 ^ "と" ^ kanji2 ^ "はつながっていません"
          else
            kanji1 ^ "から" ^ kanji2 ^ "までは" ^ string_of_float distance ^ "kmです";;

let test1 = display_distance "myougadani" "shinotsuka"
	    = "myougadaniという駅は存在しません"
let test2 = display_distance "myogadani" "shinotsuka"
	    = "茗荷谷から新大塚までは1.2kmです"
let test3 = display_distance "myogadani" "ikebukuro"
	    = "茗荷谷と池袋はつながっていません"
let test4 = display_distance "tokyo" "ootemachi"
	    = "ootemachiという駅は存在しません"
let test5 = display_distance "tokyo" "otemachi"
	    = "東京から大手町までは0.6kmです"