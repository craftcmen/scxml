-record(state, {id,
                initial,
                onentry = [],
                onexit = [],
                transition = [],
                state = [],
                final = false}).
-record(transition, {event, 'cond', target, type, execute = []}).
-record(send, {id, event, target, delay = 0}).
-record(log, {label}).
-record(raise, {event}).
-record(cancel, {sendid}).
-record(assign, {location, expr}).
-record(script, {content}).
-record('if', {'cond', then = [], 'else' = []}).
