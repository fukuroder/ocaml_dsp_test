(************************************************************************************************
 * ocaml_dsp_test.ml
 *
 * Created by fukuroda (https://github.com/fukuroder)
 *
 * ocamlfind ocamlopt -o ocaml_dsp_test ocaml_dsp_test.ml -package alsa,yojson,core -thread -linkpkg
 ************************************************************************************************)

open Core.Std

(********************************
 * 共通
 ********************************)

(* 零ベクトル作成 *)
let rec zero_vector size = (
    match size with
    | 0 -> []
    | _ -> 0.0::(zero_vector (size-1))
)

(* 単位ベクトル作成 *)
let unit_vector size index = (
    zero_vector index @ (1.0::zero_vector (size-index-1))
)

(********************************
 * 接続情報ファイル読み込み
 ********************************)

(* JSON例外 *)
exception NotSupportedJSONFormat

(* id取得 *)
let get_id obj = (
    obj |> Yojson.Basic.Util.member "id" |> Yojson.Basic.Util.to_string
)

(* operator取得 *)
let get_operator obj = (
    obj |> Yojson.Basic.Util.member "operator" |> Yojson.Basic.Util.to_string
)

(* value取得 *)
let get_value obj = (
    obj |> Yojson.Basic.Util.member "value" |> Yojson.Basic.Util.to_number
)

(* input取得 *)
let get_inputs obj = (
    obj |> Yojson.Basic.Util.member "inputs"
        |> Yojson.Basic.Util.to_list
        |> List.map ~f:Yojson.Basic.Util.to_string
)

(* idで検索 *)
let find_object_by_id ~j:json id = (
    json |> Yojson.Basic.Util.member "objects"
         |> Yojson.Basic.Util.to_list
         |> List.find_exn ~f:(fun x->(get_id x) = id)
)

(* operatorで検索 *)
let find_objects_by_text ~j:json text = (
    json |> Yojson.Basic.Util.member "objects"
         |> Yojson.Basic.Util.to_list
         |> List.filter ~f:(fun x->(get_operator x) = text)
         |> List.sort ~cmp:(fun x y->compare (get_id x) (get_id y))
)

(* z^-1のindex取得 *)
let z_index_of ~j:json z_obj = (
    let z_id = get_id z_obj in
        find_objects_by_text ~j:json "z^-1"
        |> List.findi ~f:(fun _ x-> (get_id x) = z_id)
        |> function
           | Some(i, _) -> i
           | None       -> -1 (* error *)
)

(* ツリー構造 *)
type tree =
| AudioIn            (* オーディオ信号入力 *)
| ZIn of int         (* 遅延オーディオ信号入力 *)
| Const of float     (* 定数 *)
| SampleRate         (* サンプリングレート *)
| Add of tree * tree (* 加算 *)
| Sub of tree * tree (* 減算 *)
| Mul of tree * tree (* 乗算 *)
| Div of tree * tree (* 除算 *)
| Tan of tree        (* 三角関数のtan *)
| AudioOut of tree   (* オーディオ信号出力 *)
| ZOut of tree       (* 遅延オーディオ信号出力 *)

(* ツリー作成 *)
let create_tree ~j:json terminal_id = (
    let rec create_tree2 id = (
        let obj = find_object_by_id ~j:json id in
        match (get_operator obj) with
        | "audioin"     -> AudioIn
        | "const"       -> Const(get_value obj)
        | "z^-1"        -> ZIn(z_index_of ~j:json obj)
        | "samplerate"  -> SampleRate
        | _ as operator ->
            (match (operator, get_inputs obj) with
            | ("add", x::y::[]) -> Add(create_tree2 x, create_tree2 y)
            | ("sub", x::y::[]) -> Sub(create_tree2 x, create_tree2 y)
            | ("mul", x::y::[]) -> Mul(create_tree2 x, create_tree2 y)
            | ("div", x::y::[]) -> Div(create_tree2 x, create_tree2 y)
            | ("tan", x::[])    -> Tan(create_tree2 x)
            | _                 -> raise NotSupportedJSONFormat
            )
    ) in match (get_operator terminal_id, get_inputs terminal_id) with
         | ("audioout", x::[]) -> AudioOut(create_tree2 x)
         | ("z^-1", x::[])     -> ZOut(create_tree2 x)
         | _                   -> raise NotSupportedJSONFormat
)

(********************************
 * 接続解析
 ********************************)

(* 信号タイプ *)
type signal =
| AudioSignal of (float list) (* オーディオ入力信号・遅延信号 *)
| ConstSignal of float        (* 定数信号 *)

(* ベクトル作成 *)
let create_vector ~z_count:z_count terminal_obj = (
    let rec create_vector2 out = (
        match out with
        | AudioIn      -> AudioSignal(1.0::(zero_vector z_count))
        | ZIn(index)   -> AudioSignal(0.0::(unit_vector z_count index))
        | Const(value) -> ConstSignal(value)
        | SampleRate   -> ConstSignal(44100.0)
        | Add(x, y)    ->
            (match (create_vector2 x, create_vector2 y) with
            | (AudioSignal(a), AudioSignal(b)) -> AudioSignal(List.map2_exn ~f:(+.) a b)
            | (ConstSignal(a), ConstSignal(b)) -> ConstSignal(a +. b)
            | (AudioSignal(_), ConstSignal(_)) -> raise NotSupportedJSONFormat
            | (ConstSignal(_), AudioSignal(_)) -> raise NotSupportedJSONFormat
            )
        | Sub(x, y) ->
            (match (create_vector2 x, create_vector2 y) with
            | (AudioSignal(a), AudioSignal(b)) -> AudioSignal(List.map2_exn ~f:(-.) a b)
            | (ConstSignal(a), ConstSignal(b)) -> ConstSignal(a -. b)
            | (AudioSignal(_), ConstSignal(_)) -> raise NotSupportedJSONFormat
            | (ConstSignal(_), AudioSignal(_)) -> raise NotSupportedJSONFormat
            )
        | Mul(x, y) ->
            (match (create_vector2 x, create_vector2 y) with
            | (AudioSignal(_), AudioSignal(_)) -> raise NotSupportedJSONFormat
            | (ConstSignal(a), ConstSignal(b)) -> ConstSignal(a *. b)
            | (AudioSignal(a), ConstSignal(b)) -> AudioSignal(List.map ~f:(fun x->x*.b) a)
            | (ConstSignal(a), AudioSignal(b)) -> AudioSignal(List.map ~f:(fun x->a*.x) b)
            )
        | Div(x, y) ->
            (match (create_vector2 x, create_vector2 y) with
            | (AudioSignal(_), AudioSignal(_)) -> raise NotSupportedJSONFormat
            | (ConstSignal(a), ConstSignal(b)) -> ConstSignal(a /. b)
            | (AudioSignal(a), ConstSignal(b)) -> AudioSignal(List.map ~f:(fun x->x/.b) a)
            | (ConstSignal(_), AudioSignal(_)) -> raise NotSupportedJSONFormat
            )
        | Tan(x) ->
            (match (create_vector2 x) with
            | AudioSignal(_) -> raise NotSupportedJSONFormat
            | ConstSignal(a) -> ConstSignal(tan a)
            )
        | AudioOut(x) -> create_vector2 x
        | ZOut(x)     -> create_vector2 x
    ) in match (create_vector2 terminal_obj) with
         | AudioSignal(a) -> a
         | ConstSignal(_) -> raise NotSupportedJSONFormat
)

(* a, [z1,z2,...] *)
let get_matrix jsonfile = (
    let json = Yojson.Basic.from_file jsonfile in
    let z_count = List.length (find_objects_by_text ~j:json "z^-1") in
        List.hd_exn (find_objects_by_text ~j:json "audioout")
        |> create_tree ~j:json
        |> create_vector ~z_count:z_count,
        find_objects_by_text ~j:json "z^-1"
        |> List.map ~f:(create_tree ~j:json)
        |> List.map ~f:(create_vector ~z_count:z_count)
)

(********************************
 * WAVファイル読み込み
 ********************************)

(* WAV例外 *)
exception NotSupportedWAVFormat of string

(* 文字列読み込み *)
let read_string f n = (
    let buffer = Buffer.create n in (
        Buffer.add_channel buffer f n;
        Buffer.contents buffer
    )
)

(* 4byte読み込み *)
let read_byte4 f = (
    let rec read_byte4_sub n = (
        let v = Int64.of_int (input_byte f) in
        let v = Int64.shift_left v (n*8) in
        (match n with
        | 3 ->  v
        | _ ->  Int64.bit_or v (read_byte4_sub (n+1))
        )
    ) in read_byte4_sub 0
)

(* 2byte読み込み *)
let read_byte2 f = (
    let a1 = input_byte f and a2 = input_byte f in (
       (a2 lsl 8) lor a1
    )
)

(* WAVヘッダの読み込みとframe数取得 *)
let get_frames wav = (
    (* RIFFヘッダ *)
    let data = read_string wav 4 in (
        if data <> "RIFF" then raise (NotSupportedWAVFormat "RIFFでない")
    );

    (* ファイルサイズ - 8 *)
    ignore(read_byte4 wav);

    (* WAVEヘッダ *)
    let data = read_string wav 4 in (
        if data <> "WAVE" then raise (NotSupportedWAVFormat "WAVEでない")
    );

    (* fmtチャンク *)
    let data = read_string wav 4 in (
        if data <> "fmt " then raise (NotSupportedWAVFormat "fmt でない")
    );

    (* fmtチャンクのバイト数 *)
    let data = read_byte4 wav in (
        if data <> 16L then raise (NotSupportedWAVFormat "fmtチャンクのバイト数が16でない")
    );

    (* フォーマットID *)
    let data = read_byte2 wav in (
        if data <> 1 then raise (NotSupportedWAVFormat "PCMでない")
    );

    (* チャンネル数 *)
    let data = read_byte2 wav in (
        if data <> 2 then raise (NotSupportedWAVFormat "stereoでない")
    );

    (* サンプリングレート *)
    let data = read_byte4 wav in (
        if data <> 44100L then raise (NotSupportedWAVFormat "サンプリングレートが44100[kHz]でない")
    );

    (* データ速度 *)
    let data = read_byte4 wav in (
        if data <> 176400L then raise (NotSupportedWAVFormat "データ速度が176400[byte/sec]でない")
    );

    (* ブロックサイズ *)
    let data = read_byte2 wav in (
        if data <> 4 then raise (NotSupportedWAVFormat "ブロックサイズが4でない")
    );

    (* サンプルあたりのビット数 *)
    let data = read_byte2 wav in (
        if data <> 16 then raise (NotSupportedWAVFormat "サンプルあたりのビット数が16でない")
    );

    (* dataチャンク *)
    let data = read_string wav 4 in (
        if data <> "data" then raise (NotSupportedWAVFormat "dataでない")
    );

    (* 波形データのバイト数 *)
    let data = read_byte4 wav in Int64.to_int_exn (Int64.(/) data 4L) (* frame数 *)
)

(********************************
 * 音声処理
 ********************************)

(* デフォルトオーディオデバイスを開く *)
let open_default_pcm = (
    let pcm = Alsa.Pcm.open_pcm "hw:0,0" [Alsa.Pcm.Playback] [] in (
        let params = Alsa.Pcm.get_params pcm in (
            Alsa.Pcm.set_access pcm params Alsa.Pcm.Access_rw_interleaved;
            Alsa.Pcm.set_format pcm params Alsa.Pcm.Format_s16_le;
            ignore (Alsa.Pcm.set_rate_near pcm params 44100 Alsa.Dir_eq);
            Alsa.Pcm.set_channels pcm params 2;
            Alsa.Pcm.set_buffer_size pcm params (1024*4);
            Alsa.Pcm.set_params pcm params;
        );
        pcm
    )
)

(* 1サンプル読み込み *)
let read_wav_data wav_buffer frame = (
    let read_char k = (
        Char.to_int (Buffer.nth wav_buffer k)
    )
    and to_float (c1,c2) = (
        let s = (c2 lsl 8) lor c1 in
        if s >= 32768 then Int.to_float (s - 65536) (* 負 *)
        else               Int.to_float s           (* 正 *)
    ) in to_float (read_char (4*frame),   read_char (4*frame+1)),
         to_float (read_char (4*frame+2), read_char (4*frame+3))
)

(* サンプル書き込み *)
let write_alsa_buffer buffer frame ch1 ch2  = (
    (* 浮動小数点数を符号なし16ビット整数に変換 *)
    let convert_uint16 s = (
        let int16 = (
            (* 四捨五入して符号あり16ビット整数に変換 *)
            if s >= 0.0 then Float.to_int (s +. 0.5)
                        else Float.to_int (s -. 0.5)
        )
        in (
            (* 符号なし16ビット整数に変換 *)
            if      int16 >  32767 then 32767
            else if int16 < -32768 then 65535
            else if int16 >= 0     then int16         (* 正 *)
                                   else int16 + 65536 (* 負 *)
        )
    ) in (
        let a = convert_uint16 ch1 in (
            (* バッファに書き込み *)
            buffer.[4*frame]   <- char_of_int (a land 255);
            buffer.[4*frame+1] <- char_of_int ((a lsr 8) land 255)
        );
        let a = convert_uint16 ch2 in (
            (* バッファに書き込み *)
            buffer.[4*frame+2] <- char_of_int (a land 255);
            buffer.[4*frame+3] <- char_of_int ((a lsr 8) land 255)
        )
    )
)

(********************************
 * Entry
 ********************************)
let () = (
    let a_vector, z_matrix = get_matrix "test.json"
    and wav_handle = In_channel.create ~binary:true "test.wav" in
    let wav_frames = get_frames wav_handle     (* フレーム数取得 *)
    and alsa_pcm = open_default_pcm            (* オーディオデバイスを開く *)
    and alsa_buffer = String.create (2*2*1024)  (* PCMデータ転送用バッファ *)
    and wav_buffer = Buffer.create (2*2*1024)  (* WAVデータ読み込み用バッファ *) in

    let rec audio_process remain_frames z_in_ch1 z_in_ch2 = (
        (* WAVファイルからデータを1ブロック読み込み *)
        Buffer.clear wav_buffer;
        let block_size = if remain_frames > 1024 then 1024 else remain_frames in
        Buffer.add_channel wav_buffer wav_handle (block_size*4);

        (* ブロック処理 *)
        let rec audio_process_block alsa_frame z_in_ch1 z_in_ch2 = (
            if alsa_frame < block_size then (
                (* 1フレーム取得 *)
                let a_in_ch1, a_in_ch2 = read_wav_data wav_buffer alsa_frame in
                let in_ch1 = a_in_ch1::z_in_ch1
                and in_ch2 = a_in_ch2::z_in_ch2 in

                (* ch1更新 *)
                let a_out_ch1
                    = List.fold2_exn ~f:(fun a x y->a+.x*.y) ~init:0.0 in_ch1 a_vector
                and z_out_ch1
                    = List.map ~f:(List.fold2_exn ~f:(fun a x y->a+.x*.y) ~init:1.0e-100 in_ch1) z_matrix

                (* ch2更新 *)
                and a_out_ch2
                    = List.fold2_exn ~f:(fun a x y->a+.x*.y) ~init:0.0 in_ch2 a_vector
                and z_out_ch2
                    = List.map ~f:(List.fold2_exn ~f:(fun a x y->a+.x*.y) ~init:1.0e-100 in_ch2) z_matrix in

                (* PCMデータ転送用バッファ書き込み *)
                write_alsa_buffer alsa_buffer alsa_frame a_out_ch1 a_out_ch2;

                (* next frame *)
                audio_process_block (alsa_frame + 1) z_out_ch1 z_out_ch2
            )
            else z_in_ch1, z_in_ch2 (* end block *)
        ) in
        let z_out_ch1, z_out_ch2 = audio_process_block 0 z_in_ch1 z_in_ch2 in (* start block *)

        (* PCMデータ転送 *)
        ignore (Alsa.Pcm.writei alsa_pcm alsa_buffer 0 block_size);

        if remain_frames > 1024 then audio_process (remain_frames - 1024) z_out_ch1 z_out_ch2 (* next block *)
    ) in
    let z_in_init = zero_vector (List.length z_matrix) in
    audio_process wav_frames z_in_init z_in_init; (* start *)

    (* オーディオデバイスを閉じる *)
    Alsa.Pcm.drain alsa_pcm;
    Alsa.Pcm.close alsa_pcm;

    (* ファイルを閉じる *)
    In_channel.close wav_handle
)
