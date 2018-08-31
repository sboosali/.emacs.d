example_rules = '''
##################################################
# builtins
##################################################

<dgndictation> imported;
<dgnletters>   imported;

##################################################
# public non-terminals
##################################################

<dictating> exported = say   <dgndictation> [ stop ];
<spelling>  exported = spell <dgnletters>   [ stop ];
<keyboard>  exported = press <keysequence>  [ stop ];
<mouse>     exported =       <click>        [ stop ];
<commands>  exported =       <command>+     [ stop ];

##################################################
# private non-terminals
##################################################

<command> = [<number>] <action>;
<action>  = {command}; 

#------------------------------------------------#

<keysequence> = <keychord>+
              | [ <modifiers> ] <dgnletters>;
<keychord>    = [ <modifiers> ] <key> [ key ];
<key>         = {key}
              | "key-code" <number>;

#------------------------------------------------#

<click>  = [ <modifiers> ] [ {multiplier} ] [ <button> ] click;
<button> = {button}
         | "button-code" <number>;

#------------------------------------------------#

<motion> = <absolute_motion>
         | <relative_motion>
         ;

<absolute_motion> = {ordinal} {location};

<relative_motion> = {cardinal} {location};

# <motion> = <idempotent_motion>
#          | <invertible_motion>;
# <idempotent_motion> = {ordinal} {location};
# <invertible_motion> = {cardinal} {location};

#------------------------------------------------#

<modifiers> = {modifier}+;

<number> = {cardinal};

##################################################
'''