---
layout: page
toc: true
hide_hero: true
menubar: user_guide_menu
---

# Troubleshooting

Here are some common compiler errors you might encounter and suggested solutions for them while using WebGear.

## Missing Trait Constraints

```
Could not deduce (Get h (RequiredQueryParam "tag" Text) Request)
     arising from a use of ‘queryParam’
   from the context:
```

This means that you have used a middleware (or some other function) that is probing for the specified trait (in this
case `RequiredQueryParam "tag" Text`) but you have not added a constraint to the type signature of this function.

You can fix this by adding the constraint mentioned in the error message to the type signature. You can also combine
multiple `Get` constraints to a single `Gets` constraint.

You might get a similar error for missing `Set` constraints:

```
Could not deduce (Set h (Body JSON ErrorResponse) Response)
     arising from a use of ‘respondA’
```

In this case, you can add the specified constraint to the handler.

## Missing Trait Proofs

```
Could not deduce (HasTrait (RequiredQueryParam "offset" Int) req)
     arising from a use of ‘from’
```

In this case, you are missing a proof that a witnessed request has certain traits. There are a few possible fixes:

1. Make sure that the specified trait is actually required by your handler and the type
   level literals such as "offset" are spelled correctly.
2. Did you add a correct middleware in this handler? For example, this error suggests that it is looking for a
   `queryParam @"offset" @Int` middleware. Did you intend to add that middleware before invoking `from`?
3. Alternatively, you may want to add that middleware in the function calling the current handler. In that case, you can
   add the `HasTrait (RequiredQueryParam "offset" Int) req` constraint to the current handler.
   
   If you have many such handlers, use `HaveTraits [t1, t2, ....] req` instead where `t1`, `t2`, ... are traits.

