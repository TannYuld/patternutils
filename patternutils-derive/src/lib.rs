use proc_macro::TokenStream;
use syn::{
    parse::{Parse, ParseStream, Result},
    parse_macro_input,
    punctuated::Punctuated,
    Data, DeriveInput, Fields, Ident, ItemTrait, Lit, LitBool, LitStr, Meta, Token, Visibility,
};

#[derive(Debug, Default)]
struct ObserverArgs {
    publisher_name: Option<String>
}

impl Parse for ObserverArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut result: ObserverArgs = ObserverArgs::default();
        if input.is_empty() {
            return Ok(result);
        }

        let metas = Punctuated::<Meta, Token![,]>::parse_terminated(input)?;

        for meta in metas {
            let meta = match meta {
                Meta::NameValue(n) => n,
                _ => {
                    return Err(syn::Error::new_spanned(meta, "Expected name = value pairs"));
                }
            };

            let ident = match meta.path.get_ident() {
                Some(n) => n,
                None => {
                    return Err(syn::Error::new_spanned(meta.path, "Expected identifier"));
                }
            };

            if let syn::Expr::Lit(expr_lit) = meta.value {
                match ident.to_string().as_str() {
                    "publisher_name" => {
                        if let Lit::Str(lit_str) = expr_lit.lit {
                            result.publisher_name = Some(lit_str.value());
                            continue;
                        }

                        return Err(syn::Error::new_spanned(
                            meta.path,
                            "Expected type 'String' for 'publisher_name' but found something else.",
                        ));
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            meta.path,
                            "Unknown attribute. Expected 'publisher_name'",
                        ))
                    }
                }
            }
        }

        Ok(result)
    }
}


/// Derives a builder pattern for the struct.
///
/// #### Container Attribute: `#[builder_attr(...)]`
///
/// - `name = "BuilderName"`: Renames the generated builder struct.
/// - `build_by = "value" | "reference"`: Chooses between value-consuming or reference-based build method.
/// - `opt_in`: Requires fields to be explicitly marked with `#[builder_field(include = true)]`.
///
/// #### Field Attribute: `#[builder_field(...)]`
///
/// - `name = "method_name"`: Renames the generated builder method for this field.
/// - `include = true | false`: Controls whether the field is included in the builder (overrides `opt_in`).
///
/// # Example
/// ```rust
/// #[derive(Builder)]
/// #[builder_attr(build_by = "reference", opt_in)]
/// struct User {
///     #[builder_field(name = "set_name", include = true)]
///     name: String,
///     password: String,
/// }
/// ```
/// 
/// To be included in the builder pattern, a field must implement the `Default` trait.
/// Fields not included in the builder must be explicitly provided as arguments to the final `build` method.
/// 
/// ```rust
/// #[derive(Builder)]
/// #[builder_attr(name = "PersonMaker")]
/// struct Person{
///     weight: f32,
///     age: u16,
///     name: &'static str,
/// 
///     #[builder_field(include = false)]
///     callback: fn(i32) -> i32,
/// }
/// 
/// let person = Person::builder()
///     .age(37)
///     .weight(72.45)
///     .name("Tenma")
///     .build(|num| num + 5);
/// 
/// assert_eq!(person.age, 37);
/// assert_eq!(person.weight, 72.45);
/// assert_eq!(person.name, "Tenma");
/// assert_eq!((person.callback)(5), 10);
/// ```
#[proc_macro_derive(Builder, attributes(builder_attr, builder_field))]
pub fn builder_derive_macro(item: TokenStream) -> TokenStream {
    let astree: DeriveInput = match syn::parse(item) {
        Ok(n) => n,
        Err(_) => {
            let msg = "An error occured.";
            return quote::quote! {
                compile_error!(#msg);
            }
            .into();
        }
    };

    let mut custom_builder_name: Option<String> = None;
    let mut build_method: String = String::from("value");
    let mut default_exclude = false;

    for attr in astree.attrs {
        if attr.path().is_ident("builder_attr") {
            match attr.parse_nested_meta(|meta| {

                // #builder_attr(name = N)
                if meta.path.is_ident("name") {
                    let lit: LitStr = meta.value()?.parse()?;
                    custom_builder_name = Some(lit.value());
                    return Ok(());
                }

                // #builder_attr(build_by = N)
                if meta.path.is_ident("build_by") {
                    let lit: LitStr = meta.value()?.parse()?;
                    let method_val = lit.value();
                    match method_val.as_str() {
                        "reference" | "value" => {
                            build_method = method_val;
                        },
                        _ => {
                            return Err(meta.error("`build_by` attribute can get only one of `reference | value` as parameter."));
                        }
                    }
                    return Ok(());
                }

                // #builder_attr(opt_in)
                if meta.path.is_ident("opt_in") {
                    default_exclude = true;
                    return Ok(());
                }

                Err(meta.error("unrecognized attribute"))
            }) {
                Ok(_) => {}
                Err(e) => {
                    let msg = e.to_string();
                    return quote::quote! {
                    compile_error!(#msg);
                    }
                    .into();
                }
            }
        }
    }

    let original_ident = astree.ident;
    let builder_ident = match custom_builder_name {
        Some(n) => syn::Ident::new(&n, original_ident.span()),
        None => syn::Ident::new(&format!("{}Builder", original_ident), original_ident.span()),
    };

    let data_structure = match &astree.data {
        Data::Struct(n) => n,
        _ => {
            return quote::quote! {
                compile_error!("`#[derive(Builder)]` can only be used with structs.");
            }
            .into();
        }
    };

    let fields = match &data_structure.fields {
        Fields::Named(n) => n.named.clone(),
        Fields::Unit => {
            return quote::quote! {
                compile_error!("`#[derive(Builder)]` cannot be used with unit structs.");
            }
            .into();
        }
        Fields::Unnamed(_) => {
            return quote::quote! {
                compile_error!("`#[derive(Builder)]` cannot be used with tuple structs.");
            }
            .into();
        }
    };

    let mut included_field_names: Vec<syn::Ident> = vec![];
    let mut excluded_field_names: Vec<syn::Ident> = vec![];

    let mut included_original_field_names: Vec<syn::Ident> = vec![];
    let mut excluded_original_field_names: Vec<syn::Ident> = vec![];

    let mut included_field_types: Vec<&syn::Type> = vec![];
    let mut excluded_field_types: Vec<&syn::Type> = vec![];

    for field in fields.iter() {
        let original_name = match field.ident.as_ref() {
            Some(n) => n.to_string(),
            None => {
                let msg = "Error occured";
                return quote::quote! {
                    compile_error!(#msg)
                }
                .into();
            }
        };
        let mut name = original_name.clone();
        let mut include: Option<bool> = None;

        for attr in field.attrs.iter() {
            if attr.path().is_ident("builder_field") {
                match attr.parse_nested_meta(|meta| {
                    // #builder_field(name = "N")
                    if meta.path.is_ident("name") {
                        let lit: LitStr = meta.value()?.parse()?;
                        name = lit.value().clone();
                        return Ok(());
                    }

                    // #builder_field(include = true | false)
                    if meta.path.is_ident("include") {
                        let lit: LitBool = meta.value()?.parse()?;
                        include = Some(lit.value());

                        return Ok(());
                    }

                    Err(meta.error("unrecognized attribute"))
                }) {
                    Ok(_) => {}
                    Err(e) => {
                        let msg = e.to_string();
                        return quote::quote! {
                            compile_error!(#msg);
                        }
                        .into();
                    }
                }
            }
        }

        let span = match field.ident.as_ref() {
            Some(n) => n.span(),
            None => {
                return quote::quote! {
                    compile_error!("An error occured");
                }
                .into();
            }
        };

        let should_include = match include {
            Some(n) => n,
            None => !default_exclude,
        };

        if should_include {
            included_field_names.push(syn::Ident::new(&name, span));
            included_original_field_names.push(syn::Ident::new(&original_name, span));
            included_field_types.push(&field.ty);
        } else {
            excluded_field_names.push(syn::Ident::new(&name, span));
            excluded_original_field_names.push(syn::Ident::new(&original_name, span));
            excluded_field_types.push(&field.ty);
        }
    }

    let mut expanded = quote::quote! {
        impl #original_ident{

            #[doc = "Creates and returns a new instance of the builder for this struct."]
            fn builder() -> #builder_ident{
                #builder_ident::default()
            }
        }

        #[doc = "The builder struct derives the `Default` trait, so every included field must also implement `Default`."]
        #[derive(Default)]
        pub struct #builder_ident {
            #(#included_field_names: #included_field_types),*
        }
    };

    if build_method == "value" {
        expanded.extend(quote::quote! {
            impl #builder_ident{
                #(
                    pub fn #included_field_names(self, #included_field_names:#included_field_types) -> Self {
                        #builder_ident{
                            #included_field_names:#included_field_names,
                            ..self
                        }
                    }
                )*

                pub fn build(self, #( #excluded_field_names : #excluded_field_types ),* ) -> #original_ident{
                    #original_ident {
                        #(
                            #included_original_field_names: self.#included_field_names.clone(),
                        )*
                        #(
                            #excluded_original_field_names: #excluded_field_names,
                        )*
                    }
                }
            }
        });
    } else {
        expanded.extend(quote::quote! {
            impl #builder_ident{
                #(
                    pub fn #included_field_names(&mut self, #included_field_names:#included_field_types) -> &mut Self {
                        self.#included_field_names = #included_field_names;
                        self
                    }
                )*

                #[doc = "The `build` method constructs the entire struct using the builder's internal values for included fields. Any remaining fields must be provided as arguments to this method."]
                pub fn build(&self, #( #excluded_field_names : #excluded_field_types ),*) -> #original_ident{
                    #original_ident {
                        #(
                            #included_original_field_names: self.#included_field_names.clone(),
                        )*
                        #(
                            #excluded_original_field_names: #excluded_field_names.clone(),
                        )*
                    }
                }
            }
        });
    }

    expanded.into()
}

/// Implements the `event(&mut self)` & `as_any(&self)` methodes to applied trait
/// and generate Observer struct to use observer pattern.
///
/// ### Supported Attributes:
/// - `publisher_name = "name"` = change the name of the generated publisher struct to spesific value.
/// 
/// ### Generates
/// (for trait `MyTrait`)
///
/// ```rust
/// trait MyTrait{
///     ...
///     fn event(&mut self);
///     fn as_any(&self) -> &dyn Any;
/// }
///
/// struct MyTraitObserver{
///     observers: Vec<Box<dyn MyTrait>>
/// }
///
/// impl MyTraitObserver{
///     fn new() -> MyTraitObserver {...}
///     fn get_observer_ptr(observer: &Box<dyn MyTrait>) -> *const dyn MyTrait {...}
///
///     fn subscribe(&mut self, val: Box<dyn MyTrait>) {...}
///     fn unsubscribe(&mut self, target: *const dyn MyTrait) {...}
///     fn notify(&mut self) {...}
/// }
/// ```
///
/// # Example
///
/// ```rust
/// use patternutils::observer;
///
/// #[observer(publisher_name = "ItemPublisher")]
/// trait ItemObserver {
///     fn get_code(&self) -> usize;
/// }
///
/// struct Item {
///     code: usize
/// }
///
/// impl ItemObserver for Item {
///     fn get_code(&self) -> usize {
///         self.code
///     }
///
///     fn event(&mut self) {
///         println!("[{}] Event!", self.code);
///     }
///
///     fn as_any(&self) ->  &dyn Any {
///         self
///     }
/// }
///
/// fn main() {
///     let mut last_ptr = None;
///     let mut publisher = ItemPublisher::new();
///
///     for i in 0..10{
///         let boxed_item: Box<dyn ItemObserver> = Box::new(Item {code: i});
///         last_ptr = Some(ItemPublisher::get_observer_ptr(&boxed_item));
///         publisher.subscribe(boxed_item);            //Add instance to observers
///     }
///     
///     println!("Every item:");
///     publisher.notify();                             //Notify all instances
///     
///     println!("\nEvery item without the last one:");
///
///     publisher.unsubscribe(last_ptr.unwrap());       //Remove last added instance
///     publisher.notify();                             //Notify all instances
/// }
/// ```
#[proc_macro_attribute]
pub fn observer(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as ItemTrait);
    let args = parse_macro_input!(attr as ObserverArgs);

    let pub_state = match input.vis {
        Visibility::Public(_) => Some(quote::quote! {pub}),
        _ => None,
    };

    let trait_name = &input.ident;
    let publisher_name: Ident = match args.publisher_name {
        Some(n) => Ident::new(&n, trait_name.span()),
        None => Ident::new(
            &(trait_name.to_string() + &"Publisher".to_owned()),
            trait_name.span(),
        ),
    };

    let event_doc_line = format!(
        "Internal method which will invoked when its publisher (`{}`) calls `notify()` method.",
        publisher_name
    );

    let methods = quote::quote! {
        #[doc = #event_doc_line]
        fn event(&mut self);
        fn as_any(&self) -> &dyn ::std::any::Any;
    };

    let dummy_trait: syn::ItemTrait = syn::parse2(quote::quote! {
        trait __Dummy {
            #methods
        }
    })
    .expect("Failed to parse methods");
    input.items.extend(dummy_trait.items);

    let publisher_struct_doc = format!("Publisher struct of instances which implements `{}`. This can be used to send invoke messages, add and remove observers.", trait_name);

    quote::quote! {
        #input

        #[doc = #publisher_struct_doc]
        #[doc = "# Example"]
        #[doc = "```rust"]
        #[doc = "let mut publisher = MyPublisher::new();"]
        #[doc = "publisher.subscribe(Box::new(my_var));"]
        #[doc = "publisher.notify();"]
        #[doc = ""]
        #[doc = "```"]
        #pub_state struct #publisher_name{
            observers: Vec<Box<dyn #trait_name>>
        }

        impl #publisher_name{
            #[doc = "Push the boxed instance into observers list."]
            #pub_state fn subscribe(&mut self, val: Box<dyn #trait_name>){
                self.observers.push(val);
            }

            #[doc = "Remove the instance by its boxed pointer reference from the observers list."]
            #[doc = ""]
            #[doc = "# Example"]
            #[doc = "```rust"]
            #[doc = "use patternutils::observer;"]
            #[doc = ""]
            #[doc = "#[observer(publisher_name = \"FooPublisher\")]"]
            #[doc = "trait FooObserver {}"]
            #[doc = "struct Foo {code: usize}"]
            #[doc = "impl FooObserver for Foo {"]
            #[doc = "   fn event(&mut self) {"]
            #[doc = "       println!(\"[{}] Event!\", self.code);"]
            #[doc = "   }"]
            #[doc = ""]
            #[doc = "   fn as_any(&self) -> &dyn ::std::any::Any {"]
            #[doc = "       self"]
            #[doc = "   }"]
            #[doc = "}"]
            #[doc = ""]
            #[doc = "let mut publisher = FooPublisher::new();"]
            #[doc = "let instance = Foo{code: 3};"]
            #[doc = ""]
            #[doc = "let boxed_val: Box<dyn FooObserver> = Box::new(instance);"]
            #[doc = "let ptr = FooPublisher::get_observer_ptr(&boxed_val);"]
            #[doc = ""]
            #[doc = "publisher.subscribe(boxed_val);"]
            #[doc = ""]
            #[doc = "println!(\"before\");"]
            #[doc = "publisher.notify();"]
            #[doc = "publisher.unsubscribe(ptr);"]
            #[doc = ""]
            #[doc = "println!(\"after\");"]
            #[doc = "publisher.notify();"]
            #[doc = "```"]
            #pub_state fn unsubscribe(&mut self, target: *const dyn #trait_name){
                self.observers.retain(|obs| {
                    let ptr: *const dyn #trait_name = &**obs;
                    ptr != target
                });
            }

            #[doc = "Invoke every instance which are observing this publisher."]
            #pub_state fn notify(&mut self){
                for item in self.observers.iter_mut() {
                    item.event();
                }
            }

            #pub_state fn new() -> #publisher_name {
                #publisher_name{
                    observers: vec![]
                }
            }

            #[doc = "Get pointer referece of boxed trait implentation."]
            #pub_state fn get_observer_ptr(observer: &Box<dyn #trait_name>) -> *const dyn #trait_name {
                &**observer
            }
        }
    }
    .into()
}
