-- Taller: Gestión de Inventario en Haskell

-- Estructura del inventario: lista de tuplas (Nombre, Precio, Cantidad)
type Inventory = [(String, Double, Int)]

-- 1. Agregar un nuevo producto
addProduct :: Inventory -> String -> Double -> Int -> Inventory
addProduct inventory name price quantity = inventory ++ [(name, price, quantity)]   -- Añade al final

-- 2. Actualizar cantidad de un producto existente
updateQuantity :: Inventory -> String -> Int -> Inventory
updateQuantity [] _ _ = []   -- Caso base: inventario vacío
updateQuantity ((n, p, q):xs) name newQuantity
    | n == name = (n, p, newQuantity) : xs                    -- Si coincide, reemplaza cantidad
    | otherwise = (n, p, q) : updateQuantity xs name newQuantity  -- Si no, sigue buscando

-- 3. Eliminar un producto del inventario
removeProduct :: Inventory -> String -> Inventory
removeProduct inventory name = filter (\(n, _, _) -> n /= name) inventory   -- Filtra quitando el producto

-- 4. Resumen: total de productos en stock y valor total
inventorySummary :: Inventory -> (Int, Double)
inventorySummary inventory = (totalQuantity, totalValue)
  where
    totalQuantity = sum [q | (_, _, q) <- inventory]               -- Suma todas las cantidades
    totalValue    = sum [p * fromIntegral q | (_, p, q) <- inventory] -- Precio * cantidad

-- ===== Expansión =====

-- 5. Buscar un producto
findProduct :: Inventory -> String -> Maybe (Double, Int)
findProduct [] _ = Nothing   -- Si no encuentra, devuelve Nothing
findProduct ((n, p, q):xs) name
    | n == name = Just (p, q)        -- Si coincide, devuelve precio y cantidad
    | otherwise = findProduct xs name -- Si no, sigue buscando

-- 6. Aplicar descuento a todos los productos (ej. 10% -> 0.10)
applyDiscount :: Inventory -> Double -> Inventory
applyDiscount inventory discount = [(n, p * (1 - discount), q) | (n, p, q) <- inventory]  -- Reduce precios

-- ===== Pruebas =====
main :: IO ()
main = do
    let inventory = []   -- Inventario vacío
    let inventory1 = addProduct inventory "Manzanas" 0.5 100   -- Agrega manzanas
    let inventory2 = addProduct inventory1 "Platanos" 0.3 150  -- Agrega plátanos
    let inventory3 = updateQuantity inventory2 "Manzanas" 120  -- Actualiza cantidad de manzanas
    let inventory4 = removeProduct inventory3 "Platanos"       -- Elimina plátanos
    let (totalQty, totalValue) = inventorySummary inventory4   -- Obtiene resumen

    putStrLn $ "Inventario Final: " ++ show inventory4         -- Muestra inventario
    putStrLn $ "Total de productos en stock: " ++ show totalQty -- Muestra total de cantidades
    putStrLn $ "Valor total del inventario: " ++ show totalValue -- Muestra valor total

    -- Probar búsqueda
    case findProduct inventory4 "Manzanas" of
        Just (p, q) -> putStrLn $ "Producto encontrado: Precio = " ++ show p ++ ", Cantidad = " ++ show q
        Nothing     -> putStrLn "Producto no encontrado."

    -- Probar descuento del 20%
    let discounted = applyDiscount inventory4 0.20
    putStrLn $ "Inventario con 20% de descuento: " ++ show discounted

