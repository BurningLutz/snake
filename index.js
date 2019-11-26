// Generated by purs bundle 0.13.5
var PS = {};
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Apply"] = $PS["Control.Apply"] || {};
  var exports = $PS["Control.Apply"];                    
  var Apply = function (Functor0, apply) {
      this.Functor0 = Functor0;
      this.apply = apply;
  };                      
  var apply = function (dict) {
      return dict.apply;
  };
  exports["Apply"] = Apply;
  exports["apply"] = apply;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Applicative"] = $PS["Control.Applicative"] || {};
  var exports = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];        
  var Applicative = function (Apply0, pure) {
      this.Apply0 = Apply0;
      this.pure = pure;
  };
  var pure = function (dict) {
      return dict.pure;
  };
  var liftA1 = function (dictApplicative) {
      return function (f) {
          return function (a) {
              return Control_Apply.apply(dictApplicative.Apply0())(pure(dictApplicative)(f))(a);
          };
      };
  };
  exports["Applicative"] = Applicative;
  exports["pure"] = pure;
  exports["liftA1"] = liftA1;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Bind"] = $PS["Control.Bind"] || {};
  var exports = $PS["Control.Bind"];                       
  var Discard = function (discard) {
      this.discard = discard;
  };
  var Bind = function (Apply0, bind) {
      this.Apply0 = Apply0;
      this.bind = bind;
  };
  var discard = function (dict) {
      return dict.discard;
  };                     
  var bind = function (dict) {
      return dict.bind;
  };
  var composeKleisli = function (dictBind) {
      return function (f) {
          return function (g) {
              return function (a) {
                  return bind(dictBind)(f(a))(g);
              };
          };
      };
  };
  var discardUnit = new Discard(function (dictBind) {
      return bind(dictBind);
  });
  exports["Bind"] = Bind;
  exports["bind"] = bind;
  exports["discard"] = discard;
  exports["composeKleisli"] = composeKleisli;
  exports["discardUnit"] = discardUnit;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Semigroupoid"] = $PS["Control.Semigroupoid"] || {};
  var exports = $PS["Control.Semigroupoid"];
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Category"] = $PS["Control.Category"] || {};
  var exports = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];                
  var Category = function (Semigroupoid0, identity) {
      this.Semigroupoid0 = Semigroupoid0;
      this.identity = identity;
  };
  var identity = function (dict) {
      return dict.identity;
  };
  var categoryFn = new Category(function () {
      return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
      return x;
  });
  exports["identity"] = identity;
  exports["categoryFn"] = categoryFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Monad"] = $PS["Control.Monad"] || {};
  var exports = $PS["Control.Monad"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];                
  var Monad = function (Applicative0, Bind1) {
      this.Applicative0 = Applicative0;
      this.Bind1 = Bind1;
  };
  var ap = function (dictMonad) {
      return function (f) {
          return function (a) {
              return Control_Bind.bind(dictMonad.Bind1())(f)(function (v) {
                  return Control_Bind.bind(dictMonad.Bind1())(a)(function (v1) {
                      return Control_Applicative.pure(dictMonad.Applicative0())(v(v1));
                  });
              });
          };
      };
  };
  exports["Monad"] = Monad;
  exports["ap"] = ap;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Monad.Error.Class"] = $PS["Control.Monad.Error.Class"] || {};
  var exports = $PS["Control.Monad.Error.Class"];                
  var MonadThrow = function (Monad0, throwError) {
      this.Monad0 = Monad0;
      this.throwError = throwError;
  };
  var throwError = function (dict) {
      return dict.throwError;
  };
  exports["throwError"] = throwError;
  exports["MonadThrow"] = MonadThrow;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Monad.Trans.Class"] = $PS["Control.Monad.Trans.Class"] || {};
  var exports = $PS["Control.Monad.Trans.Class"];
  var MonadTrans = function (lift) {
      this.lift = lift;
  };
  var lift = function (dict) {
      return dict.lift;
  };
  exports["lift"] = lift;
  exports["MonadTrans"] = MonadTrans;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.Functor"] = $PS["Data.Functor"] || {};
  var exports = $PS["Data.Functor"];               
  var Functor = function (map) {
      this.map = map;
  };
  var map = function (dict) {
      return dict.map;
  };
  exports["Functor"] = Functor;
  exports["map"] = map;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.Either"] = $PS["Data.Either"] || {};
  var exports = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];          
  var Left = (function () {
      function Left(value0) {
          this.value0 = value0;
      };
      Left.create = function (value0) {
          return new Left(value0);
      };
      return Left;
  })();
  var Right = (function () {
      function Right(value0) {
          this.value0 = value0;
      };
      Right.create = function (value0) {
          return new Right(value0);
      };
      return Right;
  })();
  var functorEither = new Data_Functor.Functor(function (f) {
      return function (m) {
          if (m instanceof Left) {
              return new Left(m.value0);
          };
          if (m instanceof Right) {
              return new Right(f(m.value0));
          };
          throw new Error("Failed pattern match at Data.Either (line 38, column 1 - line 38, column 52): " + [ m.constructor.name ]);
      };
  });
  var either = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Left) {
                  return v(v2.value0);
              };
              if (v2 instanceof Right) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Either (line 238, column 1 - line 238, column 64): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  exports["Left"] = Left;
  exports["Right"] = Right;
  exports["either"] = either;
  exports["functorEither"] = functorEither;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.Newtype"] = $PS["Data.Newtype"] || {};
  var exports = $PS["Data.Newtype"];                                   
  var Newtype = function (unwrap, wrap) {
      this.unwrap = unwrap;
      this.wrap = wrap;
  };
  var unwrap = function (dict) {
      return dict.unwrap;
  };
  exports["unwrap"] = unwrap;
  exports["Newtype"] = Newtype;
})(PS);
(function(exports) {
  "use strict";

  exports.pureE = function (a) {
    return function () {
      return a;
    };
  };

  exports.bindE = function (a) {
    return function (f) {
      return function () {
        return f(a())();
      };
    };
  };
})(PS["Effect"] = PS["Effect"] || {});
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Effect"] = $PS["Effect"] || {};
  var exports = $PS["Effect"];
  var $foreign = $PS["Effect"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Data_Functor = $PS["Data.Functor"];                    
  var monadEffect = new Control_Monad.Monad(function () {
      return applicativeEffect;
  }, function () {
      return bindEffect;
  });
  var bindEffect = new Control_Bind.Bind(function () {
      return applyEffect;
  }, $foreign.bindE);
  var applyEffect = new Control_Apply.Apply(function () {
      return functorEffect;
  }, Control_Monad.ap(monadEffect));
  var applicativeEffect = new Control_Applicative.Applicative(function () {
      return applyEffect;
  }, $foreign.pureE);
  var functorEffect = new Data_Functor.Functor(Control_Applicative.liftA1(applicativeEffect));
  exports["functorEffect"] = functorEffect;
  exports["monadEffect"] = monadEffect;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Effect.Class"] = $PS["Effect.Class"] || {};
  var exports = $PS["Effect.Class"];
  var Control_Category = $PS["Control.Category"];
  var Effect = $PS["Effect"];                
  var MonadEffect = function (Monad0, liftEffect) {
      this.Monad0 = Monad0;
      this.liftEffect = liftEffect;
  };
  var monadEffectEffect = new MonadEffect(function () {
      return Effect.monadEffect;
  }, Control_Category.identity(Control_Category.categoryFn));
  var liftEffect = function (dict) {
      return dict.liftEffect;
  };
  exports["liftEffect"] = liftEffect;
  exports["MonadEffect"] = MonadEffect;
  exports["monadEffectEffect"] = monadEffectEffect;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Monad.Except.Trans"] = $PS["Control.Monad.Except.Trans"] || {};
  var exports = $PS["Control.Monad.Except.Trans"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Apply = $PS["Control.Apply"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad = $PS["Control.Monad"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Effect_Class = $PS["Effect.Class"];                
  var ExceptT = function (x) {
      return x;
  };
  var newtypeExceptT = new Data_Newtype.Newtype(function (n) {
      return n;
  }, ExceptT);
  var monadTransExceptT = new Control_Monad_Trans_Class.MonadTrans(function (dictMonad) {
      return function (m) {
          return Control_Bind.bind(dictMonad.Bind1())(m)(function (v) {
              return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(v));
          });
      };
  });
  var mapExceptT = function (f) {
      return function (v) {
          return f(v);
      };
  };
  var functorExceptT = function (dictFunctor) {
      return new Data_Functor.Functor(function (f) {
          return mapExceptT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Either.functorEither)(f)));
      });
  };
  var monadExceptT = function (dictMonad) {
      return new Control_Monad.Monad(function () {
          return applicativeExceptT(dictMonad);
      }, function () {
          return bindExceptT(dictMonad);
      });
  };
  var bindExceptT = function (dictMonad) {
      return new Control_Bind.Bind(function () {
          return applyExceptT(dictMonad);
      }, function (v) {
          return function (k) {
              return Control_Bind.bind(dictMonad.Bind1())(v)(Data_Either.either((function () {
                  var $98 = Control_Applicative.pure(dictMonad.Applicative0());
                  return function ($99) {
                      return $98(Data_Either.Left.create($99));
                  };
              })())(function (a) {
                  var v1 = k(a);
                  return v1;
              }));
          };
      });
  };
  var applyExceptT = function (dictMonad) {
      return new Control_Apply.Apply(function () {
          return functorExceptT(((dictMonad.Bind1()).Apply0()).Functor0());
      }, Control_Monad.ap(monadExceptT(dictMonad)));
  };
  var applicativeExceptT = function (dictMonad) {
      return new Control_Applicative.Applicative(function () {
          return applyExceptT(dictMonad);
      }, (function () {
          var $100 = Control_Applicative.pure(dictMonad.Applicative0());
          return function ($101) {
              return ExceptT($100(Data_Either.Right.create($101)));
          };
      })());
  };
  var monadEffectExceptT = function (dictMonadEffect) {
      return new Effect_Class.MonadEffect(function () {
          return monadExceptT(dictMonadEffect.Monad0());
      }, (function () {
          var $102 = Control_Monad_Trans_Class.lift(monadTransExceptT)(dictMonadEffect.Monad0());
          var $103 = Effect_Class.liftEffect(dictMonadEffect);
          return function ($104) {
              return $102($103($104));
          };
      })());
  };
  var monadThrowExceptT = function (dictMonad) {
      return new Control_Monad_Error_Class.MonadThrow(function () {
          return monadExceptT(dictMonad);
      }, (function () {
          var $110 = Control_Applicative.pure(dictMonad.Applicative0());
          return function ($111) {
              return ExceptT($110(Data_Either.Left.create($111)));
          };
      })());
  };
  exports["newtypeExceptT"] = newtypeExceptT;
  exports["applicativeExceptT"] = applicativeExceptT;
  exports["bindExceptT"] = bindExceptT;
  exports["monadTransExceptT"] = monadTransExceptT;
  exports["monadEffectExceptT"] = monadEffectExceptT;
  exports["monadThrowExceptT"] = monadThrowExceptT;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.Symbol"] = $PS["Data.Symbol"] || {};
  var exports = $PS["Data.Symbol"];      
  var SProxy = (function () {
      function SProxy() {

      };
      SProxy.value = new SProxy();
      return SProxy;
  })();
  var IsSymbol = function (reflectSymbol) {
      this.reflectSymbol = reflectSymbol;
  };
  var reflectSymbol = function (dict) {
      return dict.reflectSymbol;
  };
  exports["IsSymbol"] = IsSymbol;
  exports["reflectSymbol"] = reflectSymbol;
  exports["SProxy"] = SProxy;
})(PS);
(function(exports) {
  "use strict";

  // module Partial.Unsafe

  exports.unsafePartial = function (f) {
    return f();
  };
})(PS["Partial.Unsafe"] = PS["Partial.Unsafe"] || {});
(function(exports) {
  "use strict";

  // module Partial

  exports.crashWith = function () {
    return function (msg) {
      throw new Error(msg);
    };
  };
})(PS["Partial"] = PS["Partial"] || {});
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Partial"] = $PS["Partial"] || {};
  var exports = $PS["Partial"];
  var $foreign = $PS["Partial"];
  exports["crashWith"] = $foreign.crashWith;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Partial.Unsafe"] = $PS["Partial.Unsafe"] || {};
  var exports = $PS["Partial.Unsafe"];
  var $foreign = $PS["Partial.Unsafe"];
  var Partial = $PS["Partial"];
  var unsafeCrashWith = function (msg) {
      return $foreign.unsafePartial(function (dictPartial) {
          return Partial.crashWith()(msg);
      });
  };
  exports["unsafeCrashWith"] = unsafeCrashWith;
})(PS);
(function(exports) {
  "use strict";

  exports.unsafeHas = function (label) {
    return function (rec) {
      return {}.hasOwnProperty.call(rec, label);
    };
  };

  exports.unsafeGet = function (label) {
    return function (rec) {
      return rec[label];
    };
  };
})(PS["Record.Unsafe"] = PS["Record.Unsafe"] || {});
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Record.Unsafe"] = $PS["Record.Unsafe"] || {};
  var exports = $PS["Record.Unsafe"];
  var $foreign = $PS["Record.Unsafe"];
  exports["unsafeHas"] = $foreign.unsafeHas;
  exports["unsafeGet"] = $foreign.unsafeGet;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.Variant"] = $PS["Data.Variant"] || {};
  var exports = $PS["Data.Variant"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Partial_Unsafe = $PS["Partial.Unsafe"];
  var Record_Unsafe = $PS["Record.Unsafe"];
  var onMatch = function (dictRowToList) {
      return function (dictVariantMatchCases) {
          return function (dictUnion) {
              return function (r) {
                  return function (k) {
                      return function (v) {
                          if (Record_Unsafe.unsafeHas(v.type)(r)) {
                              return Record_Unsafe.unsafeGet(v.type)(r)(v.value);
                          };
                          return k(v);
                      };
                  };
              };
          };
      };
  };
  var inj = function (dictCons) {
      return function (dictIsSymbol) {
          return function (p) {
              return function (value) {
                  return {
                      type: Data_Symbol.reflectSymbol(dictIsSymbol)(p),
                      value: value
                  };
              };
          };
      };
  };
  var case_ = function (r) {
      return Partial_Unsafe.unsafeCrashWith("Data.Variant: pattern match failure [" + (r.type + "]"));
  };
  exports["inj"] = inj;
  exports["onMatch"] = onMatch;
  exports["case_"] = case_;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Control.Monad.Except.Checked"] = $PS["Control.Monad.Except.Checked"] || {};
  var exports = $PS["Control.Monad.Except.Checked"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Category = $PS["Control.Category"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Control_Monad_Trans_Class = $PS["Control.Monad.Trans.Class"];
  var Data_Either = $PS["Data.Either"];
  var Data_Functor = $PS["Data.Functor"];
  var Data_Newtype = $PS["Data.Newtype"];
  var Data_Variant = $PS["Data.Variant"];                
  var safe = function (dictFunctor) {
      var $9 = Data_Functor.map(dictFunctor)(Data_Either.either(Data_Variant.case_)(Control_Category.identity(Control_Category.categoryFn)));
      var $10 = Data_Newtype.unwrap(Control_Monad_Except_Trans.newtypeExceptT);
      return function ($11) {
          return $9($10($11));
      };
  };
  var handleError = function (dictRowToList) {
      return function (dictVariantMatchCases) {
          return function (dictUnion) {
              return function (dictMonad) {
                  return function (cases) {
                      return Control_Bind.composeKleisli(Control_Monad_Except_Trans.bindExceptT(dictMonad))((function () {
                          var $12 = Control_Monad_Trans_Class.lift(Control_Monad_Except_Trans.monadTransExceptT)(dictMonad);
                          var $13 = Data_Newtype.unwrap(Control_Monad_Except_Trans.newtypeExceptT);
                          return function ($14) {
                              return $12($13($14));
                          };
                      })())(Data_Either.either(Data_Variant.onMatch()()()(cases)(Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(dictMonad))))(Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))));
                  };
              };
          };
      };
  };
  exports["handleError"] = handleError;
  exports["safe"] = safe;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.Boolean"] = $PS["Data.Boolean"] || {};
  var exports = $PS["Data.Boolean"];
  var otherwise = true;
  exports["otherwise"] = otherwise;
})(PS);
(function(exports) {
  "use strict";

  exports.showIntImpl = function (n) {
    return n.toString();
  };

  exports.showStringImpl = function (s) {
    var l = s.length;
    return "\"" + s.replace(
      /[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
      function (c, i) {
        switch (c) {
          case "\"":
          case "\\":
            return "\\" + c;
          case "\x07": return "\\a";
          case "\b": return "\\b";
          case "\f": return "\\f";
          case "\n": return "\\n";
          case "\r": return "\\r";
          case "\t": return "\\t";
          case "\v": return "\\v";
        }
        var k = i + 1;
        var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty;
      }
    ) + "\"";
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.Show"] = $PS["Data.Show"] || {};
  var exports = $PS["Data.Show"];
  var $foreign = $PS["Data.Show"];
  var Show = function (show) {
      this.show = show;
  };
  var showString = new Show($foreign.showStringImpl);
  var showInt = new Show($foreign.showIntImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showInt"] = showInt;
  exports["showString"] = showString;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Data.GamingArea"] = $PS["Data.GamingArea"] || {};
  var exports = $PS["Data.GamingArea"];
  var Control_Applicative = $PS["Control.Applicative"];
  var Control_Monad_Error_Class = $PS["Control.Monad.Error.Class"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_Boolean = $PS["Data.Boolean"];
  var Data_Show = $PS["Data.Show"];
  var Data_Symbol = $PS["Data.Symbol"];
  var Data_Variant = $PS["Data.Variant"];                
  var GamingArea = (function () {
      function GamingArea(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      GamingArea.create = function (value0) {
          return function (value1) {
              return new GamingArea(value0, value1);
          };
      };
      return GamingArea;
  })();
  var widthOutOfRange = Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
      return "widthOutOfRange";
  }))(Data_Symbol.SProxy.value)("width should be between 2 and 100");
  var showGamingArea = new Data_Show.Show(function (v) {
      return "GamingArea(" + (Data_Show.show(Data_Show.showInt)(v.value0) + (", " + (Data_Show.show(Data_Show.showInt)(v.value1) + ")")));
  });
  var heightOutOfRange = Data_Variant.inj()(new Data_Symbol.IsSymbol(function () {
      return "heightOutOfRange";
  }))(Data_Symbol.SProxy.value)("height should be between 2 and 100");
  var gamingArea = function (dictMonad) {
      return function (w) {
          return function (h) {
              if (w < 2 || w > 100) {
                  return Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(dictMonad))(widthOutOfRange);
              };
              if (h < 2 || h > 100) {
                  return Control_Monad_Error_Class.throwError(Control_Monad_Except_Trans.monadThrowExceptT(dictMonad))(heightOutOfRange);
              };
              if (Data_Boolean.otherwise) {
                  return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(dictMonad))(new GamingArea(w, h));
              };
              throw new Error("Failed pattern match at Data.GamingArea (line 32, column 1 - line 36, column 67): " + [ w.constructor.name, h.constructor.name ]);
          };
      };
  };
  exports["gamingArea"] = gamingArea;
  exports["showGamingArea"] = showGamingArea;
})(PS);
(function(exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  var Data_Show = $PS["Data.Show"];
  var logShow = function (dictShow) {
      return function (a) {
          return $foreign.log(Data_Show.show(dictShow)(a));
      };
  };
  exports["logShow"] = logShow;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Effect.Class.Console"] = $PS["Effect.Class.Console"] || {};
  var exports = $PS["Effect.Class.Console"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Console = $PS["Effect.Console"];
  var logShow = function (dictMonadEffect) {
      return function (dictShow) {
          var $23 = Effect_Class.liftEffect(dictMonadEffect);
          var $24 = Effect_Console.logShow(dictShow);
          return function ($25) {
              return $23($24($25));
          };
      };
  };
  exports["logShow"] = logShow;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.5
  "use strict";
  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var Control_Bind = $PS["Control.Bind"];
  var Control_Monad_Except_Checked = $PS["Control.Monad.Except.Checked"];
  var Control_Monad_Except_Trans = $PS["Control.Monad.Except.Trans"];
  var Data_GamingArea = $PS["Data.GamingArea"];
  var Data_Show = $PS["Data.Show"];
  var Effect = $PS["Effect"];
  var Effect_Class = $PS["Effect.Class"];
  var Effect_Class_Console = $PS["Effect.Class.Console"];                
  var logGamingArea = function (dictMonadEffect) {
      return function (w) {
          return function (h) {
              return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(dictMonadEffect.Monad0()))(Data_GamingArea.gamingArea(dictMonadEffect.Monad0())(w)(h))(function (v) {
                  return Effect_Class_Console.logShow(Control_Monad_Except_Trans.monadEffectExceptT(dictMonadEffect))(Data_GamingArea.showGamingArea)(v);
              });
          };
      };
  };
  var main = (function () {
      var handleAll = Control_Monad_Except_Checked.handleError()()()(Effect.monadEffect)({
          widthOutOfRange: function (s) {
              return Effect_Class_Console.logShow(Control_Monad_Except_Trans.monadEffectExceptT(Effect_Class.monadEffectEffect))(Data_Show.showString)(s);
          },
          heightOutOfRange: function (s) {
              return Effect_Class_Console.logShow(Control_Monad_Except_Trans.monadEffectExceptT(Effect_Class.monadEffectEffect))(Data_Show.showString)(s);
          }
      });
      return Control_Monad_Except_Checked.safe(Effect.functorEffect)(Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(handleAll(logGamingArea(Effect_Class.monadEffectEffect)(0)(20)))(function () {
          return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(handleAll(logGamingArea(Effect_Class.monadEffectEffect)(10)(200)))(function () {
              return handleAll(logGamingArea(Effect_Class.monadEffectEffect)(20)(30));
          });
      }));
  })();
  exports["logGamingArea"] = logGamingArea;
  exports["main"] = main;
})(PS);
PS["Main"].main();