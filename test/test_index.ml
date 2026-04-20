open OUnit2
open Hit

(* Set up a clean index directory for a test. The try wrapper handles
   first-run when the directory does not yet exist. *)
let setup_index suffix =
  let dir = Printf.sprintf "./test_idx_%s" suffix in
  (try Io.delete_all_files ~predicate:(Fun.const true) dir with _ -> ());
  (dir, Index.create dir)

(* Paths of the form "/d/x" where x is a single character produce no path
   tokens (all segments are shorter than min_token_length = 2), so content
   tokens start at position 0 in the combined stream. This keeps position
   arithmetic predictable across tests. *)

let tests =
  [
    (* ----------------------------------------------------------------- *)
    (* IndexNewDocument                                                   *)
    (* ----------------------------------------------------------------- *)
    ( "IndexNewDocument: returns stable doc_id derived from path" >:: fun _ ->
      let _, idx = setup_index "stable_id" in
      let doc_id = Index.add idx "/tmp/doc01.txt" "Document 01 - 1 2 3" in
      assert_equal ~printer:Fun.id "doc-8cc92cb296ec50f6a41ea3d7711ca06f"
        (Doc.Id.to_string doc_id) );

    ( "IndexNewDocument: content tokens are searchable" >:: fun _ ->
      let _, idx = setup_index "content_tokens" in
      let doc_id = Index.add idx "/d/a" "hello world" in
      let results = Index.query idx "(eq hello)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string doc_id (List.hd results) );

    ( "IndexNewDocument: path tokens are searchable" >:: fun _ ->
      let _, idx = setup_index "path_tokens" in
      let doc_id = Index.add idx "/docs/readme.txt" "" in
      let results = Index.query idx "(eq docs)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string doc_id (List.hd results) );

    ( "IndexNewDocument: tokens shorter than min_token_length are not indexed"
    >:: fun _ ->
      let _, idx = setup_index "min_token_length" in
      let _ = Index.add idx "/d/a" "a bb ccc" in
      (* "a" has length 1, below the minimum of 2 *)
      let short = Index.query idx "(eq a)" in
      assert_equal ~printer:string_of_int 0 (List.length short);
      (* "bb" has length 2 and must be indexed *)
      let ok = Index.query idx "(eq bb)" in
      assert_equal ~printer:string_of_int 1 (List.length ok) );

    ( "IndexNewDocument: tokens are lowercased" >:: fun _ ->
      let _, idx = setup_index "lowercase" in
      let doc_id = Index.add idx "/d/a" "Hello WORLD" in
      let results = Index.query idx "(eq hello)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string doc_id (List.hd results) );

    (* ----------------------------------------------------------------- *)
    (* AddDocumentIdempotent                                              *)
    (* ----------------------------------------------------------------- *)
    ( "AddDocumentIdempotent: same content returns same doc_id" >:: fun _ ->
      let _, idx = setup_index "idempotent_id" in
      let id1 = Index.add idx "/d/a" "hello world" in
      let id2 = Index.add idx "/d/a" "hello world" in
      assert_equal ~printer:Doc.Id.to_string id1 id2 );

    ( "AddDocumentIdempotent: document still found after no-op add" >:: fun _ ->
      let _, idx = setup_index "idempotent_query" in
      let doc_id = Index.add idx "/d/a" "hello world" in
      let _ = Index.add idx "/d/a" "hello world" in
      let results = Index.query idx "(eq hello)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string doc_id (List.hd results) );

    (* ----------------------------------------------------------------- *)
    (* ReindexDocument                                                    *)
    (* ----------------------------------------------------------------- *)
    ( "ReindexDocument: returns same doc_id after content change" >:: fun _ ->
      (* doc_id is derived from path, so it is stable across updates *)
      let _, idx = setup_index "reindex_id" in
      let id1 = Index.add idx "/d/a" "hello world" in
      let id2 = Index.add idx "/d/a" "goodbye world" in
      assert_equal ~printer:Doc.Id.to_string id1 id2 );

    ( "ReindexDocument: new tokens become searchable" >:: fun _ ->
      let _, idx = setup_index "reindex_new" in
      let doc_id = Index.add idx "/d/a" "hello world" in
      let _ = Index.add idx "/d/a" "goodbye world" in
      let results = Index.query idx "(eq goodbye)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string doc_id (List.hd results) );

    ( "ReindexDocument: stale tokens no longer match" >:: fun _ ->
      let _, idx = setup_index "reindex_stale" in
      let _ = Index.add idx "/d/a" "uniqueoldtoken" in
      let _ = Index.add idx "/d/a" "uniquenewtoken" in
      let results = Index.query idx "(eq uniqueoldtoken)" in
      assert_equal ~printer:string_of_int 0 (List.length results) );

    ( "ReindexDocument: tokens shared with old content remain searchable"
    >:: fun _ ->
      let _, idx = setup_index "reindex_shared" in
      let doc_id = Index.add idx "/d/a" "shared uniqueold" in
      let _ = Index.add idx "/d/a" "shared uniquenew" in
      let results = Index.query idx "(eq shared)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string doc_id (List.hd results) );

    (* ----------------------------------------------------------------- *)
    (* DeleteDocument                                                     *)
    (* ----------------------------------------------------------------- *)
    ( "DeleteDocument: document not returned by query after deletion"
    >:: fun _ ->
      let _, idx = setup_index "delete_query" in
      let doc_id = Index.add idx "/d/a" "hello world" in
      Index.delete idx doc_id;
      let results = Index.query idx "(eq hello)" in
      assert_equal ~printer:string_of_int 0 (List.length results) );

    ( "DeleteDocument: other documents are unaffected" >:: fun _ ->
      let _, idx = setup_index "delete_others" in
      let id_a = Index.add idx "/d/a" "hello world" in
      let id_b = Index.add idx "/d/b" "hello earth" in
      Index.delete idx id_a;
      let results = Index.query idx "(eq hello)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string id_b (List.hd results) );

    (* ----------------------------------------------------------------- *)
    (* QueryIndex - Eq                                                    *)
    (* ----------------------------------------------------------------- *)
    ( "QueryIndex Eq: returns document containing the term" >:: fun _ ->
      let _, idx = setup_index "eq_found" in
      let doc_id = Index.add idx "/d/a" "searchterm" in
      let results = Index.query idx "(eq searchterm)" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string doc_id (List.hd results) );

    ( "QueryIndex Eq: returns empty list for unknown term" >:: fun _ ->
      let _, idx = setup_index "eq_not_found" in
      let _ = Index.add idx "/d/a" "hello world" in
      let results = Index.query idx "(eq unknown)" in
      assert_equal ~printer:string_of_int 0 (List.length results) );

    (* ----------------------------------------------------------------- *)
    (* QueryIndex - Or                                                    *)
    (* ----------------------------------------------------------------- *)
    ( "QueryIndex Or: returns union of matching documents" >:: fun _ ->
      let _, idx = setup_index "or_union" in
      let id_a = Index.add idx "/d/a" "foo" in
      let id_b = Index.add idx "/d/b" "bar" in
      let results = Index.query idx "(or (eq foo) (eq bar))" in
      assert_equal ~printer:string_of_int 2 (List.length results);
      assert_bool "doc_a in results" (List.mem id_a results);
      assert_bool "doc_b in results" (List.mem id_b results) );

    ( "QueryIndex Or: returns empty when no clause matches" >:: fun _ ->
      let _, idx = setup_index "or_empty" in
      let _ = Index.add idx "/d/a" "hello" in
      let results = Index.query idx "(or (eq foo) (eq bar))" in
      assert_equal ~printer:string_of_int 0 (List.length results) );

    ( "QueryIndex Or: document matching multiple clauses appears once"
    >:: fun _ ->
      let _, idx = setup_index "or_dedup" in
      let _ = Index.add idx "/d/a" "foo bar" in
      let results = Index.query idx "(or (eq foo) (eq bar))" in
      assert_equal ~printer:string_of_int 1 (List.length results) );

    (* ----------------------------------------------------------------- *)
    (* QueryIndex - And                                                   *)
    (* ----------------------------------------------------------------- *)
    ( "QueryIndex And: returns only documents matching all terms" >:: fun _ ->
      let _, idx = setup_index "and_intersection" in
      let id_ab = Index.add idx "/d/a" "foo bar" in
      let _ = Index.add idx "/d/b" "foo" in
      let _ = Index.add idx "/d/c" "bar" in
      let results = Index.query idx "(and (eq foo) (eq bar))" in
      assert_equal ~printer:string_of_int 1 (List.length results);
      assert_equal ~printer:Doc.Id.to_string id_ab (List.hd results) );

    ( "QueryIndex And: returns empty when any term is absent" >:: fun _ ->
      let _, idx = setup_index "and_absent" in
      let _ = Index.add idx "/d/a" "foo" in
      let results = Index.query idx "(and (eq foo) (eq missing))" in
      assert_equal ~printer:string_of_int 0 (List.length results) );

    (* ----------------------------------------------------------------- *)
    (* Scoring                                                            *)
    (* Paths "/d/x" produce no path tokens, so content positions         *)
    (* start at 0. Scoring: (1 + count) * (1 + 1/(1 + span))            *)
    (* ----------------------------------------------------------------- *)
    ( "Scoring: higher token frequency ranks first for single-term query"
    >:: fun _ ->
      (* doc_b: "foo" at positions [0;1;2], count=3, span=0, score=8
         doc_a: "foo" at position  [0],     count=1, span=0, score=4 *)
      let _, idx = setup_index "score_frequency" in
      let id_a = Index.add idx "/d/a" "foo" in
      let id_b = Index.add idx "/d/b" "foo foo foo" in
      let results = Index.query idx "(eq foo)" in
      assert_equal ~printer:string_of_int 2 (List.length results);
      assert_equal ~printer:Doc.Id.to_string id_b (List.nth results 0);
      assert_equal ~printer:Doc.Id.to_string id_a (List.nth results 1) );

    ( "Scoring: closer term proximity ranks first for multi-term query"
    >:: fun _ ->
      (* doc_a: "foo bar"                — foo@0, bar@1, span=1, score=4
         doc_b: "foo something else bar" — foo@0, bar@3, span=3, score=3 *)
      let _, idx = setup_index "score_proximity" in
      let id_a = Index.add idx "/d/a" "foo bar" in
      let id_b = Index.add idx "/d/b" "foo something else bar" in
      let results = Index.query idx "(and (eq foo) (eq bar))" in
      assert_equal ~printer:string_of_int 2 (List.length results);
      assert_equal ~printer:Doc.Id.to_string id_a (List.nth results 0);
      assert_equal ~printer:Doc.Id.to_string id_b (List.nth results 1) );

    (* ----------------------------------------------------------------- *)
    (* get_doc                                                            *)
    (* ----------------------------------------------------------------- *)
    ( "get_doc: returns document with correct path" >:: fun _ ->
      let _, idx = setup_index "get_doc" in
      let doc_id = Index.add idx "/d/a" "hello" in
      let doc = Index.get_doc idx doc_id in
      assert_equal ~printer:Fun.id "/d/a" (Doc.path doc) );
  ]

let _ = run_test_tt_main ("Index" >::: tests)
