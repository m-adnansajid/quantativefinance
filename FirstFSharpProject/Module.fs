
namespace Namespace.Library1

module MainModule =

    let x = 2
    let y = 3

    module NestedModule =
        let f = x + y ///The f function in the module NestedModulecan access the values x and y in the parent module, without explicitly writing out its name.

    printfn "%A" NestedModule.f

    // module Module1 = 
    //     let x = Module2.Version()  //Error: Not yet declared!
    
    // module Module2 = 
    //     let Version() = "Version 1.0"
    
    //Reverse the order and the error is resolved.

    module Module2 = 
        let Version() = "Version 1.0"
    

namespace Namespace1.Library2
    module Module1 = 
        let x = Namespace.Library1.MainModule.Module2.Version()  //Error: Not yet declared!
    
