data Item = Item { nome :: String, preco :: Int } deriving Show

main :: IO ()
main = do
  putStrLn "Kit de Sobrevivência"
  menu []
    
    
menu :: [Item] -> IO ()
menu lista = do
  putStrLn "\nMenu"
  putStrLn "1. Adicionar item ao kit"
  putStrLn "2. Listar todos os itens no kit"
  putStrLn "3. Remover item do kit"
  putStrLn "4. Calcular custo total do kit"
  putStrLn "5. Sair da aplicação"
  opc <- getLine
  case opc of
    "1" -> adicionarItem lista
    "2" -> listarItens lista
    "3" -> removerItem lista
    "4" -> calculaCusto lista
    "5" -> putStrLn "Saindo..."
    _   -> putStrLn "Insira uma opção válida." >> menu lista


-- Função para Adcionar Itens

adicionarItem :: [Item] -> IO ()
adicionarItem lista = do
  putStrLn "Digite o nome do item: "
  nomeItem <- getLine
    
  putStrLn "Digite o preço do item: "
  precoItem <- readLn
  
  let item = Item nomeItem precoItem
  let nova = lista ++ [item]
  
  putStrLn $ "O item " ++ nomeItem ++ " foi adicionado ao kit."
  menu nova


-- Função de Listar Itens 
  
listarItens :: [Item] -> IO ()
listarItens lista = do
  if null lista
    then putStrLn "O kit está vazio."
    else do
      putStrLn "Itens no kit:"
      imprimirItens lista
  menu lista

imprimirItens :: [Item] -> IO ()
imprimirItens [] = return ()
imprimirItens (Item nome preco:xs) = do
  putStrLn $ "- " ++ nome ++ ": R$" ++ show preco
  imprimirItens xs
  

-- Função de Remover Itens

removerItem :: [Item] -> IO ()
removerItem lista = do
  if null lista
  then putStrLn "O kit está vazio."
  else do
    putStrLn "Informe o nome do item a ser removido"
    item <- getLine
    
    let nova = removeNome item lista
    
    if length nova == length lista
      then putStrLn $ "O item \"" ++ item ++ "\" não foi encontrado no kit."
      else putStrLn $ "O item \"" ++ item ++ "\" foi removido do kit."
    
    menu nova
    
  menu lista

removeNome :: String -> [Item] -> [Item]
removeNome _ [] = []
removeNome nome (x:xs)
  | nome == item x = xs
  | otherwise = x : removeNome nome xs
  where item (Item nome _) = nome
  
  
-- Função para Calcular custo dos Itens

calculaCusto :: [Item] -> IO ()
calculaCusto lista = do
  if null lista
  then putStrLn "O kit está vazio."
  else do
    let custoTotal = sum (map preco lista)
    putStrLn $ "O custo total do kit é: R$ " ++ show custoTotal
  menu lista
