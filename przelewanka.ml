(* Mikolaj Grzywacz 394321
CR: Konrad Staniszewski *)

let rec nwd a b = 
    if a = 0 then b
    else nwd (b mod a) a

exception Found of int 

let rec nwd_arr arr =
    Array.fold_left (fun acc x -> nwd x acc ) 0 arr

module Hashtb = Hashtbl.Make (struct
    type t = int array
    let equal = (=)
    let hash t = Array.fold_left (fun acc x -> acc * 1000000007 + x) 0 t
end)

let przelewanka given_arr =
    (*eliminujemy puste kubki*)
    let arr = Array.of_list (List.filter (fun a -> a <> (0,0)) 
        (Array.to_list given_arr)) in
    let arr_lgh = Array.length arr in
    (*Tablica pojemnosci i koncowa*)
    let volume = Array.init arr_lgh (fun i -> fst arr.(i)) in
    let result_arr = Array.init arr_lgh (fun i -> snd arr.(i)) in
    let mozliwe = (*Sprawdzanie czy zawsze jest jakis kubeczek jest pusty/pelny*)
        let czy = ref false in
        begin
        if arr_lgh = 0 then
            czy := true
        else
            for i = 0 to (arr_lgh - 1) do
                if result_arr.(i) = volume.(i) || result_arr.(i) = 0 then
                    czy := true
            done;
        end;
        !czy
    in
    let repated = Hashtb.create 1 in
    let queue = Queue.create () in
    let result = ref (-1) in
    let check_step arr_akt given_depth = (*updatujemy sytuacje lub konczymy*)
        if not (Hashtb.mem repated arr_akt) then
            begin
            if arr_akt = result_arr then 
                raise (Found (given_depth));
                Queue.add (Array.copy arr_akt) queue;
                Hashtb.add repated (Array.copy arr_akt) (given_depth)
            end;
    in

    try (*eliminujemy oczywiste sytuacje*)
    if (nwd_arr volume) = nwd (nwd_arr volume) (nwd_arr result_arr) && mozliwe then
    begin
        check_step (Array.make arr_lgh 0) 0;
        while (not (Queue.is_empty queue)) && !result = (-1) do
            (*Tablica, bedziemy z niej wytwarzc nowe stany*)
            let work_arr = Queue.take queue in 
            let depth = Hashtb.find repated work_arr in 
            if work_arr = result_arr then 
                result := depth
            else
                let akt = Array.copy work_arr in
                begin
                    for i = 0 to arr_lgh - 1 do
                        begin (*nalewamy i wylewamy*)
                            akt.(i) <- volume.(i);
                            check_step akt (depth+1);
                            akt.(i) <- 0;
                            check_step akt (depth+1);
                            akt.(i) <- work_arr.(i);
                        end
                    done;
                    for i = 0 to arr_lgh - 1 do
                        for j = 0 to arr_lgh - 1 do
                            if i <> j then
                            begin (*Przelewamy*)
                                akt.(j) <- min volume.(j) (work_arr.(i) + work_arr.(j));
                                akt.(i) <- max 0 (work_arr.(i) + work_arr.(j) - volume.(j));
                                check_step akt (depth+1);
                                akt.(j) <- work_arr.(j); 
                                akt.(i) <- work_arr.(i)
                            end;
                        done;
                    done;
                end;
        done;
    end;
    (-1)
    with Found x -> x