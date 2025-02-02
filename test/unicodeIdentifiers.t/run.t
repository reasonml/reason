Format file with unicode identifiers

  $ refmt ./input.re | tee output.re
  type saison =
    | Hiver
    | Été
    | Printemps
    | Automne;
  let x = Été;
  
  let x = {été|xxx|été};
  let x = {%été |xxx|};
  
  let là = ça => ça;
  
  let _ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿŠšŽžŒœŸ = "ok";
  
  type t =
    | Æsop
    | Âcre
    | Ça
    | Élégant
    | Öst
    | Œuvre;
  
  let été = "summer";
  let ça = "that";
  let straße = "street";
  let øre = "ear";
  
  /* NFD representation */
  
  let f =
    fun
    | Æsop => 1
    | Âcre => 2
    | Ça => 3
    | Élégant => 4
    | Öst => 5
    | Œuvre => 6;
  
  let l = [été, ça, straße, øre];
  
  let s = _ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿŠšŽžŒœŸ;
  
  let () =
    assert(f(Élégant) /* NFC encoded */ == 4);
  
  let () = {
    let called = ref(false);
    let élégant = /* NFC encoded */ () =>
      called := true;
    élégant /* NFD encoded */();
    assert(!called);
  };
  
  /* The following two defs should error with 'Multiple definition…' */
  module Élégant /* NFC encoded */ = {};
  module Élégant /* NFD encoded */ = {};
  
  /** Quoted strings and extensions */
  
  let x = {où|x|où};
  let ko = {%Là |x|};
  
  let x = {%âcre.name été|x|été};
  let x = {%Âcre.sub été|x|été};
  
  let x = {%âcre.m |x|};
  
  let%À.ça x = ();
  
  let x = /* {été|*)|été}*/ ();
  let y =
    /* This is not a valid quoted string delimiter: {Été|*/
    ();


Test idempotency

  $ refmt output.re
  type saison =
    | Hiver
    | Été
    | Printemps
    | Automne;
  let x = Été;
  
  let x = {été|xxx|été};
  let x = {%été |xxx|};
  
  let là = ça => ça;
  
  let _ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿŠšŽžŒœŸ = "ok";
  
  type t =
    | Æsop
    | Âcre
    | Ça
    | Élégant
    | Öst
    | Œuvre;
  
  let été = "summer";
  let ça = "that";
  let straße = "street";
  let øre = "ear";
  
  /* NFD representation */
  
  let f =
    fun
    | Æsop => 1
    | Âcre => 2
    | Ça => 3
    | Élégant => 4
    | Öst => 5
    | Œuvre => 6;
  
  let l = [été, ça, straße, øre];
  
  let s = _ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿŠšŽžŒœŸ;
  
  let () =
    assert(f(Élégant) /* NFC encoded */ == 4);
  
  let () = {
    let called = ref(false);
    let élégant = /* NFC encoded */ () =>
      called := true;
    élégant /* NFD encoded */();
    assert(!called);
  };
  
  /* The following two defs should error with 'Multiple definition…' */
  module Élégant /* NFC encoded */ = {};
  module Élégant /* NFD encoded */ = {};
  
  /** Quoted strings and extensions */
  
  let x = {où|x|où};
  let ko = {%Là |x|};
  
  let x = {%âcre.name été|x|été};
  let x = {%Âcre.sub été|x|été};
  
  let x = {%âcre.m |x|};
  
  let%À.ça x = ();
  
  let x = /* {été|*)|été}*/ ();
  let y =
    /* This is not a valid quoted string delimiter: {Été|*/
    ();

