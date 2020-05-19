open Bootstrap4

type t (w :: Type) (d :: Type) = {
     Create : string -> transaction w,
     Render : w -> xbody,
     Value : w -> signal d,
     Apply : string -> d -> transaction unit
}

con singleRowTable1 (fs :: {(Type * Type * Type)}) = option $(map snd3 fs)
con singleRowTable2 (fs :: {(Type * Type * Type)}) = option $(map fst3 fs)
fun singleRowTable [u ::: Name] [group :: Name] [auth ::: {Type}] [aks ::: {{Unit}}]
    [fs ::: {(Type * Type * Type)}] [fks ::: {{Unit}}]
    [[u] ~ auth] [[group] ~ [u = string] ++ auth]
    (fl : folder fs) (ws : $(map Widget.t' fs))
    (injs : $(map (fn p => sql_injectable p.1) fs))
    (title : string)
    (auth : sql_table ([u = string, group = bool] ++ auth) aks)
    (fs : sql_table (map fst3 fs) fks)
    (labels : $(map (fn _ => string) fs))
    : t (singleRowTable1 fs) (singleRowTable2 fs) =
      let
          fun check u =
              oneRowE1 (SELECT COUNT( * ) > 0
                        FROM auth
                        WHERE auth.{u} = {[u]}
                          AND auth.{group})
      in
          {Create = fn u =>
                       ok <- check u;
                       if not ok then
                           return None
                       else
                           r <- oneOrNoRows1 (SELECT * FROM fs);
                           v <- (case r of
                                     None =>
                                     @Monad.mapR _ [Widget.t'] [snd3]
                                      (fn [nm ::_] [p ::_] (w : Widget.t' p) =>
                                          cfg <- @Widget.configure w;
                                          @Widget.create w cfg)
                                      fl ws
                                   | Some r =>
                                     @Monad.mapR2 _ [Widget.t'] [fst3] [snd3]
                                      (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : p.1) =>
                                          cfg <- @Widget.configure w;
                                          @Widget.initialize w cfg v)
                                      fl ws r);
                           return (Some v),
           Render = fn w =>
             case w of
                 None => <xml></xml>
               | Some w => <xml>
                 <h2>{[title]}</h2>

                 {@mapX3 [fn _ => string] [Widget.t'] [snd3] [_]
                   (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
                                (lab : string) (w : Widget.t' p) (v : p.2) => <xml>
                                  <div class="form-group">
                                    <label class="control-label">{[lab]}:</label>
                                    <div>{@Widget.asWidget w v None}</div>
                                  </div>
                                </xml>) fl labels ws w}
              </xml>,
           Value = Option.mapM (@Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                 (fn [nm ::_] [p ::_] => @Widget.value) fl ws),
           Apply = fn u v =>
                      case v of
                          None => return ()
                        | Some v =>
                          ok <- check u;
                          if not ok then
                              error <xml>Access denied</xml>
                          else
                              dml (DELETE FROM fs WHERE TRUE);
                              @@Sql.easy_insert [map fst3 fs] [fks] injs (@Folder.mp fl) fs v
          }
      end

con tableWithAcl1 (kt :: Type) (fs :: {(Type * Type * Type)}) = list (kt * $(map snd3 fs))
con tableWithAcl2 (kt :: Type) (fs :: {(Type * Type * Type)}) = list (kt * $(map fst3 fs))
fun tableWithAcl [u ::: Name] [k ::: Name] [kt ::: Type] [auth ::: {Type}] [aks ::: {{Unit}}]
    [k2 ::: Name] [fs ::: {(Type * Type * Type)}] [fks ::: {{Unit}}]
    [[u] ~ [k]] [[u = string, k = kt] ~ auth] [[k2] ~ fs]
    (kinj : sql_injectable_prim kt)
    (fl : folder fs) (ws : $(map Widget.t' fs))
    (injs : $(map (fn p => sql_injectable_prim p.1) fs))
    (title : kt -> string)
    (auth : sql_table ([u = string, k = kt] ++ auth) aks)
    (fs : sql_table ([k2 = kt] ++ map fst3 fs) fks)
    (labels : $(map (fn _ => string) fs))
    : t (tableWithAcl1 kt fs) (tableWithAcl2 kt fs) =
      {Create = fn u =>
                   List.mapQueryM ({{{@sql_query1 [[]] ! ! ! !
                                      {Distinct = False,
                                       From = @@sql_left_join [[]] [[Auth = [u = string, k = kt] ++ auth]]
                                                [[Fs = map (fn t => (t, option t)) ([k2 = kt] ++ map fst3 fs)]]
                                                ! ! ! {Fs = @mp [sql_injectable_prim] [fn t => nullify t (option t)] @@nullify_prim
                                                             (@Folder.concat ! _ (@@Folder.mp [fst3] [fs] fl)) ({k2 = kinj} ++ injs)}
                                                (FROM auth) (FROM fs)
                                                (WHERE auth.{k} = fs.{k2}),
                                       Where = (WHERE auth.{u} = {[u]}),
                                       GroupBy = sql_subset_all [_],
                                       Having = (WHERE TRUE),
                                       SelectFields = sql_subset [[Auth = ([k = kt], _), Fs = (map (fn p => option p.1) fs, _)]],
                                       SelectExps = {}}}}}
                                   ORDER BY auth.{k})
                                  (fn {Auth = k, Fs = vs} =>
                                      vs <- @Monad.mapR2 _ [Widget.t'] [fn p => option p.1] [snd3]
                                             (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : option p.1) =>
                                                 cfg <- @Widget.configure w;
                                                 case v of
                                                     None => @Widget.create w cfg
                                                   | Some v => @Widget.initialize w cfg v)
                                             fl ws vs;
                                      return (k.k, vs)),
       Render = List.mapX (fn (k, w) => <xml>
         <h2>{[title k]}</h2>

         {@mapX3 [fn _ => string] [Widget.t'] [snd3] [_]
           (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
               (lab : string) (w : Widget.t' p) (v : p.2) => <xml>
                 <div class="form-group">
                   <label class="control-label">{[lab]}:</label>
                   <div>{@Widget.asWidget w v None}</div>
                 </div>
               </xml>) fl labels ws w}
       </xml>),
       Value = List.mapM (fn (k, w) =>
                             vs <- @Monad.mapR2 _ [Widget.t'] [snd3] [fst3]
                                    (fn [nm ::_] [p ::_] => @Widget.value) fl ws w;
                             return (k, vs)),
       Apply = fn u v =>
                  List.app (fn (k, v) =>
                               allowed <- oneRowE1 (SELECT COUNT( * ) > 0
                                                    FROM auth
                                                    WHERE auth.{u} = {[u]}
                                                      AND auth.{k} = {[k]});
                               if not allowed then
                                   error <xml>Access denied</xml>
                               else
                                   dml (DELETE FROM fs
                                        WHERE t.{k2} = {[k]});
                                   @@Sql.easy_insert [[k2 = kt] ++ map fst3 fs] [fks]
                                     (@mp [sql_injectable_prim] [sql_injectable]
                                       @@sql_prim (@Folder.concat ! _ (@@Folder.mp [fst3] [_] fl)) ({k2 = kinj} ++ injs))
                                     (@Folder.concat ! _ (@Folder.mp fl)) fs ({k2 = k} ++ v)) v}
      
type compose1 (a :: Type) (b :: Type) = a * b
type compose2 (a :: Type) (b :: Type) = a * b
fun compose [a1 ::: Type] [b1 ::: Type] [a2 ::: Type] [b2 ::: Type]
            (a : t a1 a2) (b : t b1 b2) : t (compose1 a1 b1) (compose2 a2 b2) = {
    Create = fn u =>
                w1 <- a.Create u;
                w2 <- b.Create u;
                return (w1, w2),
    Render = fn (w1, w2) => <xml>
      {a.Render w1}
      {b.Render w2}
    </xml>,
    Value = fn (w1, w2) =>
               w1 <- a.Value w1;
               w2 <- b.Value w2;
               return (w1, w2),
    Apply = fn u (v1, v2) =>
               a.Apply u v1;
               b.Apply u v2
}
    
functor Make(M : sig
                 type p1
                 type p2
                 val t : t p1 p2

                 val whoami : transaction (option string)
             end) = struct
    open M
    
    type a = p1

    val create =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some u => t.Create u

    fun onload _ = return ()

    fun save v =
        uo <- whoami;
        case uo of
            None => error <xml>Access denied</xml>
          | Some u => t.Apply u v

    fun render _ w = <xml>
      {t.Render w}

      <button class="btn btn-primary"
              value="Save"
              onclick={fn _ =>
                          v <- current (t.Value w);
                          rpc (save v)}/>
    </xml>

    fun notification _ = <xml></xml>

    val ui = {Create = create,
              Onload = onload,
              Render = render,
              Notification = notification}
end
