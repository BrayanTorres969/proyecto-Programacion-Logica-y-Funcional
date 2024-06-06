module Principal where


import Empleado
import System.IO (hFlush, stdout)

-- Función principal
main :: IO ()
main = do
    let empleados = []
    menu empleados

-- Función que muestra el menú y maneja la selección del usuario
menu :: [Empleado] -> IO ()
menu empleados = do
    putStrLn "\nSistema de Gestión de Empleados"
    putStrLn "1. Listar empleados"
    putStrLn "2. Crear empleado"
    putStrLn "3. Buscar empleado"
    putStrLn "4. Actualizar empleado"
    putStrLn "5. Eliminar empleado"
    putStrLn "6. Salir"
    putStr "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Lista de empleados:"
            print (listarEmpleados empleados)
            menu empleados
        "2" -> do
            empleados' <- crearEmpleadoInteractivo empleados
            menu empleados'
        "3" -> do
            buscarEmpleadoInteractivo empleados
            menu empleados
        "4" -> do
            empleados' <- actualizarEmpleadoInteractivo empleados
            menu empleados'
        "5" -> do
            empleados' <- eliminarEmpleadoInteractivo empleados
            menu empleados'
        "6" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción no válida, por favor intente nuevamente."
            menu empleados

-- Funciones auxiliares para interacción con el usuario

crearEmpleadoInteractivo :: [Empleado] -> IO [Empleado]
crearEmpleadoInteractivo empleados = do
    putStr "DNI: "
    hFlush stdout
    dni <- readLn
    putStr "Nombres: "
    hFlush stdout
    nombres <- getLine
    putStr "Apellidos: "
    hFlush stdout
    apellidos <- getLine
    putStr "Puesto: "
    hFlush stdout
    puesto <- getLine
    putStr "Salario: "
    hFlush stdout
    salario <- readLn
    let empleados' = crearEmpleado dni nombres apellidos puesto salario empleados
    putStrLn "Empleado creado exitosamente."
    return empleados'

buscarEmpleadoInteractivo :: [Empleado] -> IO ()
buscarEmpleadoInteractivo empleados = do
    putStr "DNI del empleado a buscar: "
    hFlush stdout
    dni <- readLn
    case buscarEmpleado dni empleados of
        Just emp -> putStrLn ("Empleado encontrado: " ++ show emp)
        Nothing  -> putStrLn "Empleado no encontrado."

actualizarEmpleadoInteractivo :: [Empleado] -> IO [Empleado]
actualizarEmpleadoInteractivo empleados = do
    putStr "DNI del empleado a actualizar: "
    hFlush stdout
    dni <- readLn
    case buscarEmpleado dni empleados of
        Just _ -> do
            putStr "Nuevos nombres: "
            hFlush stdout
            nuevosNombres <- getLine
            putStr "Nuevos apellidos: "
            hFlush stdout
            nuevosApellidos <- getLine
            putStr "Nuevo puesto: "
            hFlush stdout
            nuevoPuesto <- getLine
            putStr "Nuevo salario: "
            hFlush stdout
            nuevoSalario <- readLn
            let empleados' = actualizarEmpleado dni nuevosNombres nuevosApellidos nuevoPuesto nuevoSalario empleados
            putStrLn "Empleado actualizado exitosamente."
            return empleados'
        Nothing -> do
            putStrLn "Empleado no encontrado."
            return empleados


eliminarEmpleadoInteractivo :: [Empleado] -> IO [Empleado]
eliminarEmpleadoInteractivo empleados = do
    putStr "DNI del empleado a eliminar: "
    hFlush stdout
    dni <- readLn
    case buscarEmpleado dni empleados of
        Just _ -> do
            let empleados' = eliminarEmpleado dni empleados
            putStrLn "Empleado eliminado exitosamente."
            return empleados'
        Nothing -> do
            putStrLn "Empleado no encontrado."
            return empleados




