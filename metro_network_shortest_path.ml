(* ex8.5 | metro *)
(* 駅名を表すレコード型 *)
type station_name_t = {
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
(* ローマ字の駅名(string)と駅名リスト(station_name_t list)を受け取って,その駅の漢字を表記を文字列で返す *)
(* 駅名リストに名前がない場合は空文字を返す *)
(* roman_to_kanji: string -> station_name_t list -> string *)
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
      = "東京から大手町までは0.6kmです";;

(* ex12.1 | metro *)
(* Dijkstraのための現在の最短距離や経路を保持するデータ型 *)
type station_t = {
  name: string; (* 漢字の駅名 *)
  shortest_distance: float; (* 暫定最短距離 *)
  route_list: string list; (* 一つ前の駅のリスト *)
};;


(* ex12.2 | metro *)
(* ex14.11 | metro *)
(* station_name_tのリストを受け取り, station_tのリストに変換する *)
(* 無名関数とList.mapを使って実装する *)
(* make_station_list: station_name_t list -> station_t list *)
(*
  let rec make_station_list station_names = match station_names with
    []                                                        -> []
  | { kanji = kanji; kana = _; roman = _; line = _; } :: rest ->
    { name = kanji; shortest_distance = infinity; route_list = [] } :: make_station_list rest;;
 *)
let make_station_list =
  List.map
    (
      fun sn ->
        match sn with { kanji = kanji; kana = _; roman = _; line = _; } ->
          { name = kanji; shortest_distance = infinity; route_list = [] }
    );;

let station_names1 = [];;
let station_names2 = [{kanji="代々木上原"; kana="よよぎうえはら"; roman="yoyogiuehara"; line="千代田線"}];;
let station_names3 = [
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
];;

let test1 = make_station_list station_names1 = [];;
let test2 = make_station_list station_names2 = [
  { name = "代々木上原"; shortest_distance = infinity; route_list = [] };
];;
let test3 = make_station_list station_names3 = [
  { name = "表参道"; shortest_distance = infinity; route_list = [] };
  { name = "乃木坂"; shortest_distance = infinity; route_list = [] };
  { name = "赤坂"; shortest_distance = infinity; route_list = [] };
  { name = "国会議事堂前"; shortest_distance = infinity; route_list = [] };
];;

(* ex12.3 | metro *)
(* station_tのリストと始点の駅名(string)を受け取って、始点のみ
   - shortest_distance = 0
   - route_listは始点の駅名のみからなるリスト
   となるstationのリストを返す
*)
(* ex14.11 | metro *)
(* 無名関数とList.mapを使って実装する *)
(* dijkstra_init: station_t list -> string -> station_t list *)
(*
  let rec dijkstra_init stations start = match stations with
    []                                                       -> []
  | { name = n; shortest_distance = _; route_list; } as first :: rest ->
    if n = start then
      { name = n; shortest_distance = 0.; route_list = [n] } :: dijkstra_init rest start
    else
      first :: dijkstra_init rest start;;
 *)
let dijkstra_init stations start =
  List.map
    (
      fun x -> match x with
        { name = n; shortest_distance = _; route_list = _; } ->
          if start = n then
            { name = n; shortest_distance = 0.; route_list = [n] }
          else
            x
    )
    stations

let stations1 = [];;
let stations2 = [
  { name = "代々木上原"; shortest_distance = infinity; route_list = [] };
];;
let stations3 = [
  { name = "表参道"; shortest_distance = infinity; route_list = [] };
  { name = "乃木坂"; shortest_distance = infinity; route_list = [] };
  { name = "赤坂"; shortest_distance = infinity; route_list = [] };
  { name = "国会議事堂前"; shortest_distance = infinity; route_list = [] };
];;

let test1 = dijkstra_init stations1 "代々木上原" = [];;
let test2 = dijkstra_init stations2 "代々木上原" = [
  { name = "代々木上原"; shortest_distance = 0.; route_list = ["代々木上原"] };
];;

let test3 = dijkstra_init stations3 "国会議事堂前" = [
  { name = "表参道"; shortest_distance = infinity; route_list = [] };
  { name = "乃木坂"; shortest_distance = infinity; route_list = [] };
  { name = "赤坂"; shortest_distance = infinity; route_list = [] };
  { name = "国会議事堂前"; shortest_distance = 0.; route_list = ["国会議事堂前"] };
];;

(* ex12.4 | metro *)
(* station_name_tのリストと station_tの値sを受け取ってsをkanaで整列した位置に挿入する *)
(* insert_station_name: station_name_t list -> station_name_t -> station_name_t list *)
let rec insert_station_name lst station = match lst with
    [] -> [station]
  | { kana = k0; } as first :: rest ->
      match station with
        { kana = k1 } ->
          if k0 < k1 then
            first :: insert_station_name rest station
          else if k0 = k1 then
            (* station :: rest *)
            insert_station_name rest station
          else
            station :: first :: rest;;
let stations1 = [];;
let stations2 = [
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
];;

let test1 =
  insert_station_name stations1 {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"} = [{kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"}];;
let test2 =
  insert_station_name stations2 {kanji="山"; kana="やま"; roman="omotesandou"; line="千代田線"} = [
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
  {kanji="山"; kana="やま"; roman="omotesandou"; line="千代田線"}
];;

(* station_name_t型のリストを受け取って, kana順に整列させ, 重複を取り除いたstaion_name_tのリストを返す *)
(* sort_and_remove_dups: station_name_t list -> station_name_t *)
let rec sort_and_remove_dups station_names = match station_names with
    []            -> []
  | first :: rest -> insert_station_name (sort_and_remove_dups rest) first;;

let stations1 = [];;
let stations2 = [
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
];;
let stations3 = [
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="あああ線"};
];;


let test1 = sort_and_remove_dups stations1 = [];;
let test2 = sort_and_remove_dups stations2 = [
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
];;
let test3 = sort_and_remove_dups stations3 = [
  {kanji="赤坂"; kana="あかさか"; roman="akasaka"; line="千代田線"};
  {kanji="表参道"; kana="おもてさんどう"; roman="omotesandou"; line="千代田線"};
  {kanji="国会議事堂前"; kana="こっかいぎじどうまえ"; roman="kokkaigijidoumae"; line="千代田線"};
  {kanji="乃木坂"; kana="のぎざか"; roman="nogizaka"; line="千代田線"};
];;

(* ex13.6 | metro *)
let station1 = {name="池袋"; shortest_distance = infinity; route_list = []}
let station2 = {name="新大塚"; shortest_distance = 1.2; route_list = ["新大塚"; "茗荷谷"]}
let station3 = {name="茗荷谷"; shortest_distance = 0.; route_list = ["茗荷谷"]}
let station4 = {name="後楽園"; shortest_distance = infinity; route_list = []}
(* 直前に確定した駅p(station_t)と未確定の駅q(station_t)を受け取って,
   p, qが直接つながっていたらqの最短距離と手前リストを更新、つながっ
   ていなければもとのqをそのまま返す

 *)
(* update: station_t -> station_t -> station_t *)

(*
let update_station p q =
  match (p, q) with
    ({ name = p_n; shortest_distance = p_sd; route_list = p_rl },
    { name = q_n; shortest_distance = q_sd; route_list = q_rl }) ->
      let p_q_distance = get_distance p_n q_n global_ekikan_list in
      let temp_distance = p_sd +. p_q_distance in
        if temp_distance < q_sd then
          { name = q_n; shortest_distance = temp_distance; route_list = q_n :: p_rl }
        else
          q;;
let test1 = update_station station3 station1 = station1
let test2 = update_station station3 station2 = station2
let test3 = update_station station3 station3 = station3
let test4 = update_station station3 station4 = {name="後楽園"; shortest_distance = 1.8; route_list = ["後楽園"; "茗荷谷"]}
let test5 = update_station station2 station1 = {name="池袋"; shortest_distance = 3.0; route_list = ["池袋"; "新大塚"; "茗荷谷"]}
let test6 = update_station station2 station2 = station2
let test7 = update_station station2 station3 = station3
let test8 = update_station station2 station4 = station4
 *)

(* ex13.7 | metro *)
(* 直前に確定した駅p(station_t), 未確定の駅のリストv(staion_t list)を受け取り, 未確定の駅のリストの更新を行う *)
(* ex14.7 | metro *)
(* update_stationを局所関数を使うように実装 *)
(* ex14.13 | metro *)
(* 局所関数を無名関数で実装 *)
(* ex16.3 | metro *)
(* 駅名リストを引数に取るようにし、get_distanceでそれを使うようにする *)
(* update: station_t -> station_t list -> edge_t list -> station_t list *)
let update p v edge_list =
  List.map
    (
      fun q -> match (p, q) with
        ({ name = p_n; shortest_distance = p_sd; route_list = p_rl },
        { name = q_n; shortest_distance = q_sd; route_list = q_rl }) ->
          let p_q_distance = get_distance p_n q_n edge_list in
          let temp_distance = p_sd +. p_q_distance in
            if temp_distance < q_sd then
              { name = q_n; shortest_distance = temp_distance; route_list = q_n :: p_rl }
            else
              q
    )
    v;;

let lst = [station1; station2; station3; station4]

(* テスト *)
let test1 = update station2 [] [] = []
let test2 = update station2 lst global_ekikan_list = [
  {name="池袋"; shortest_distance = 3.0; route_list = ["池袋"; "新大塚"; "茗荷谷"]};
  station2;
  station3;
  station4
];;

(* ex14.12 | metro *)
let ekimei_list = [
  {kanji="池袋"; kana="いけぶくろ"; roman="ikebukuro"; line="丸ノ内線"};
  {kanji="新大塚"; kana="しんおおつか"; roman="shinotsuka"; line="丸ノ内線"};
  {kanji="茗荷谷"; kana="みょうがだに"; roman="myogadani"; line="丸ノ内線"};
  {kanji="後楽園"; kana="こうらくえん"; roman="korakuen"; line="丸ノ内線"};
  {kanji="本郷三丁目"; kana="ほんごうさんちょうめ"; roman="hongosanchome"; line="丸ノ内線"};
  {kanji="御茶ノ水"; kana="おちゃのみず"; roman="ochanomizu"; line="丸ノ内線"}
]
(*
   ex12.2 ex12.3で実装したmake_station_list, dijkstra_initは同時にできる
   station_name_tのリストと始点の漢字の駅名(string)を受け取って, station_tのリストを返す
   始点のみ距離0, route_listに自分自身を含める
   14.11で定義した無名関数との関係は合成関数である
 *)
(* make_init_station_list: station_name_t list -> string -> station_t list *)
let make_init_station_list lst start =
  List.map
    (
      fun sn -> match sn with
        { kanji = k; kana = _; roman = _; line = _ } ->
          if k = start then
            { name = k; shortest_distance = 0.; route_list = [k] }
          else
            { name = k; shortest_distance = infinity; route_list = [] }
    )
    lst;;

let test1 = make_init_station_list [] "茗荷谷" = []
let test2 = make_init_station_list ekimei_list "茗荷谷" = [
  {name="池袋"; shortest_distance = infinity; route_list = []};
  {name="新大塚"; shortest_distance = infinity; route_list = []};
  {name="茗荷谷"; shortest_distance = 0.; route_list = ["茗荷谷"]};
  {name="後楽園"; shortest_distance = infinity; route_list = []};
  {name="本郷三丁目"; shortest_distance = infinity; route_list = []};
  {name="御茶ノ水"; shortest_distance = infinity; route_list = []}
];;

(* ex15.4 *)
(* station_tのリストlstを受け取り 「最短距離最小の駅」と「最短距離最小の駅以外のリスト」のペアを返す *)
(* ex15.5 *)
(* fold_rightを用いて実装 *)
(* separate_shotests: station_t list -> station_t * station_t list *)
(*
let rec separate_shortest lst =
  let default = { name =""; shortest_distance = infinity; route_list = [] } in
    match lst with
        [] -> (default, []) (* ここには到達しないはずだが、網羅性のため定義 *)
      | first :: [] -> (first, [])
      | { shortest_distance = fsd } as first :: rest ->
          let ({ shortest_distance = rsd } as rest_shortest, others) = separate_shortest rest in
            if fsd < rsd then
              (first, rest_shortest :: others)
            else
              (rest_shortest, first :: others)
 *)

(* ex15.5 *)
(* fold_rightを用いてseparate_shortestを実装 *)
(* separate_shotests: station_t list -> station_t * station_t list *)
let separate_shortest lst =
  match lst with
      [] -> ({ name =""; shortest_distance = infinity; route_list = [] }, [])
    | first :: [] -> (first, [])
    | first :: rest ->
      let get_shortest x y = match (x, y) with
        ({ shortest_distance = xsd } as xst, ({ shortest_distance = ysd } as yst, ysts)) ->
          if xsd < ysd then
            (xst, yst :: ysts)
          else
            (yst, xst :: ysts)
      in
        List.fold_right get_shortest rest (first, [])

let stations = [
  {name="池袋"; shortest_distance = infinity; route_list = []};
  {name="新大塚"; shortest_distance = infinity; route_list = []};
  {name="茗荷谷"; shortest_distance = 0.; route_list = ["茗荷谷"]};
  {name="後楽園"; shortest_distance = 1.2; route_list = ["茗荷谷"; "本郷三丁目"]};
  {name="本郷三丁目"; shortest_distance = infinity; route_list = []};
  {name="御茶ノ水"; shortest_distance = infinity; route_list = []}
];;

let test1 = separate_shortest [] = ({ name =""; shortest_distance = infinity; route_list = [] }, []);;
let test2 = separate_shortest stations = (
  {name="茗荷谷"; shortest_distance = 0.; route_list = ["茗荷谷"]},
  [
    {name="新大塚"; shortest_distance = infinity; route_list = []};
    {name="後楽園"; shortest_distance = 1.2; route_list = ["茗荷谷"; "本郷三丁目"]};
    {name="池袋"; shortest_distance = infinity; route_list = []};
    {name="本郷三丁目"; shortest_distance = infinity; route_list = []};
    {name="御茶ノ水"; shortest_distance = infinity; route_list = []};
  ]
);;

(* ex16.4 | metro *)
(*
   station_t list型の未確定の駅のリストとedge_t list型の駅間のリストを受け取って、
   ダイクストラのアルゴリズムに従って各駅について最短距離と最短経路が正しく入ったリスト(station_t list)を返す
   未確定駅リストの中には始点となるものに対してはshortest_distance = 0. となっている駅が存在する(make_init_station_listがすでに計算されている想定)
 *)
(* dijkstra_main: station_t list -> edge_t list -> station_t list *)
(* unsettlesは再帰のたびに一つずつ小さくなるので停止する *)
let rec dijkstra_main stations edges = match stations with
        [] -> []
      | lst ->
        let (decided, unsettles) = separate_shortest lst in
        let updated = update decided unsettles edges in
          decided :: dijkstra_main updated edges;;

let eki1 = {name="池袋"; shortest_distance = infinity; route_list = []}
let eki2 = {name="新大塚"; shortest_distance = 1.2; route_list = ["新大塚"; "茗荷谷"]}
let eki3 = {name="茗荷谷"; shortest_distance = 0.; route_list = ["茗荷谷"]}
let eki4 = {name="後楽園"; shortest_distance = infinity; route_list = []}

(* 駅リストの例 *)
let lst = [eki1; eki2; eki3; eki4]

(* テスト *)
let test1 = dijkstra_main [] global_ekikan_list = []
let test2 = dijkstra_main lst global_ekikan_list =
  [{name = "茗荷谷"; shortest_distance = 0.; route_list = ["茗荷谷"]};
   {name = "新大塚"; shortest_distance = 1.2; route_list = ["新大塚"; "茗荷谷"]};
   {name = "後楽園"; shortest_distance = 1.8; route_list = ["後楽園"; "茗荷谷"]};
   {name = "池袋"; shortest_distance = 3.; route_list = ["池袋"; "新大塚"; "茗荷谷"]}]

(* ex16.5 | metro *)
(* 条件となる関数pとstation_t listを受け取って該当の駅を返す *)
(* find_station: staiton_t -> 'station list -> station_t *)
let rec find_station name lst = match lst with
    [] -> { name = ""; shortest_distance = infinity; route_list = [] }
  | { name = n; shortest_distance = _; route_list = _ } as first :: rest ->
      if n = name then
        first
      else
        find_station name rest;;

(* dijkstra: string -> string -> station_t *)
let dijkstra start goal =
  let start_kanji = roman_to_kanji start global_ekimei_list in
  let goal_kanji = roman_to_kanji goal global_ekimei_list in
  let stations = make_init_station_list global_ekimei_list start_kanji in
  let result = dijkstra_main stations global_ekikan_list in
    find_station goal_kanji result;;

let test1 = dijkstra "shibuya" "gokokuji" =
  {name = "護国寺"; shortest_distance = 9.8;
   route_list =
     ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町";
      "青山一丁目"; "表参道"; "渋谷"]}
let test2 = dijkstra "myogadani" "meguro" =
  {name = "目黒"; shortest_distance = 12.7000000000000028;
   route_list =
     ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王";
      "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]};;


(* ex17.10 | metro *)
(*
  - 駅名
  - その駅に直接つながっている駅名とその駅までの距離の組のリスト
  を持つedge_tree_t
 *)

type edge_tree_t = Empty
                 | Node of edge_tree_t * string * (string * float) list * edge_tree_t (* 駅名 * (直接つながっている駅の駅名 * 距離) *)

(* ex17.11 | metro *)
(*
  「駅名」と「駅名と距離の組のリスト」を受け取って、その駅までの距離を返す
  駅がリストから見つからない場合infinityを返す
 *)
(* assoc: string -> (string * float) list -> float *)
let rec assoc station_name name_distance_pairs = match name_distance_pairs with
  | [] -> infinity
  | (a, b) :: rest ->
      if a = station_name then
        b
      else
        assoc station_name rest

let pairs1 = [];;
let pairs2 = [("a", 1.9); ("b", 2.0)];;

let test1 = assoc "a" pairs1 = infinity;;
let test2 = assoc "a" pairs2 = 1.9;;

(* ex17.11 | metro *)
(*
  edge_tree_tとedge_tを受け取ってedge_tree_tにedge_tを挿入する
  一旦は右に追加していく
 *)

(* insert1: edge_tree_t -> string -> string -> float -> edge_tree_t *)
let rec insert1 tree src dest distance = match tree with
  | Empty -> Node (Empty, src, [(dest, distance)], Empty)
  | Node (lt, st_name, lst, rt) ->
      if src < st_name then
        Node (insert1 lt src dest distance, st_name, lst, rt)
      else if st_name < src then
        Node (lt, st_name, lst, insert1 rt src dest distance)
      else
        Node (lt, st_name, (dest, distance) :: lst, rt)

(* ex17.12 | metro *)
(* insert_edge: edge_tree_t -> edge_t -> edge_tree_t *)
let insert_edge tree edge = match edge with
  { src = s; dest = d; distance = ds } -> insert1 (insert1 tree d s ds) s d ds;;

let ekikan1 =
  {src="池袋"; dest="新大塚"; via="丸ノ内線"; distance=1.8; required_time=3}
let ekikan2 =
  {src="新大塚"; dest="茗荷谷"; via="丸ノ内線"; distance=1.2; required_time=2}
let ekikan3 =
  {src="茗荷谷"; dest="後楽園"; via="丸ノ内線"; distance=1.8; required_time=2}

(* test *)
let tree1 = insert_edge Empty ekikan1
let test1 = tree1 =
  Node (Empty, "新大塚", [("池袋", 1.8)],
	Node (Empty, "池袋", [("新大塚", 1.8)], Empty))
let tree2 = insert_edge tree1 ekikan2
let test2 = tree2 =
  Node (Empty, "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)],
	Node (Empty, "池袋", [("新大塚", 1.8)],
	      Node (Empty, "茗荷谷", [("新大塚", 1.2)], Empty)))
let tree3 = insert_edge tree2 ekikan3
let test3 = tree3 =
  Node (Node (Empty, "後楽園", [("茗荷谷", 1.8)], Empty),
	"新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)],
        Node (Empty,
	      "池袋", [("新大塚", 1.8)],
	      Node (Empty,
		    "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)],
		    Empty)))

(* ex17.13 | metro *)
(* inserts_edge: edge_tree_t -> edge_t list -> edge_tree_t *)
let inserts_edge tree edges = List.fold_right (fun edge tree -> insert_edge tree edge) edges tree

let test1 = inserts_edge Empty [ekikan1; ekikan2; ekikan3] =
  Node (Empty, "後楽園", [("茗荷谷", 1.8)],
   Node
    (Node (Empty, "新大塚", [("池袋", 1.8); ("茗荷谷", 1.2)],
      Node (Empty, "池袋", [("新大塚", 1.8)], Empty)),
    "茗荷谷", [("新大塚", 1.2); ("後楽園", 1.8)], Empty));;

(* ex17.14 *)
(* 木を使ったget_distanceに変更 *)
(* get_distance_by_tree: string -> string -> edge_tree_t -> float *)
let rec get_distance_by_tree station_kanji1 station_kanji2 edge_tree = match edge_tree with
  | Empty -> infinity
  | Node (lt, name, lst, rt) ->
    if name = station_kanji1 then
      assoc station_kanji2 lst
    else if station_kanji1 < name then
      get_distance_by_tree station_kanji1 station_kanji2 lt
    else
      get_distance_by_tree station_kanji1 station_kanji2 rt

let global_ekikan_tree = inserts_edge Empty global_ekikan_list
let test1 = get_distance_by_tree "茗荷谷" "新大塚" global_ekikan_tree = 1.2
let test2 = get_distance_by_tree "茗荷谷" "池袋" global_ekikan_tree = infinity
let test3 = get_distance_by_tree "東京" "大手町" global_ekikan_tree = 0.6
