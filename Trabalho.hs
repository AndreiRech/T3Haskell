import System.Random (randomRIO)

data Item = Item { nome :: String, preco :: Int, importancia :: Int } deriving Show

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
  putStrLn "5. Avaliar o kit"
  putStrLn "6. Testar o kit"
  putStrLn "7. Sair da aplicação"
  opc <- getLine
  case opc of
    "1" -> adicionarItem lista
    "2" -> listarItens lista
    "3" -> removerItem lista
    "4" -> calculaCusto lista
    "5" -> avaliaKit lista
    "6" -> testaKit lista
    "7" -> putStrLn "Saindo..."
    _   -> putStrLn "Insira uma opção válida." >> menu lista


-- Função para Adcionar Itens

adicionarItem :: [Item] -> IO ()
adicionarItem lista = do
  putStrLn "Digite o nome do item: "
  nomeItem <- getLine
    
  putStrLn "Digite o preço do item: "
  precoItem <- readLn

  putStrLn "Digite a importância desse item: "
  importanciaItem <- readLn
  
  let item = Item nomeItem precoItem importanciaItem
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
imprimirItens (Item nome preco importancia:xs) = do
  putStrLn $ "- " ++ nome ++ ": R$" ++ show preco ++ " : Importancia - " ++ show importancia
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
  where item (Item nome _ _) = nome
  
  
-- Função para Calcular custo dos Itens

calculaCusto :: [Item] -> IO ()
calculaCusto lista = do
  if null lista
  then putStrLn "O kit está vazio."
  else do
    let custoTotal = sum (map preco lista)
    putStrLn $ "O custo total do kit é: R$ " ++ show custoTotal
  menu lista


-- Função Especial - Avaliar o Kit

avaliaKit :: [Item] -> IO ()
avaliaKit lista = do
  if null lista
    then putStrLn "O kit está vazio."
    else do
      let mediaImportancia = calculaMediaImportancia lista

          mensagem = case mediaImportancia of
            _ | mediaImportancia < 4 -> "Reprovado: O kit é muito fraco."
              | mediaImportancia <= 7 -> "Atenção: O kit pode melhorar..."
              | otherwise -> "Aprovado: O kit está bem preparado!"

      putStrLn $ "A média de importância dos itens é: " ++ show mediaImportancia
      putStrLn mensagem
  menu lista

-- Função Especial - Testar o Kit

testaKit :: [Item] -> IO ()
testaKit lista = do
  if null lista
    then putStrLn "O kit está vazio."
    else do
      let mediaImportancia = calculaMediaImportancia lista

      valorAleatorio <- randomRIO (1, 20) :: IO Double
      let resultado = mediaImportancia + valorAleatorio
      putStrLn $ "Resultado do teste: " ++ show resultado

      if resultado > 17
        then putStrLn "Parabéns! Você conseguiu sobreviver a nossa simulação com o kit!"
        else putStrLn "Não deu. O kit não foi suficiente para sobreviver a simulação. Recomendamos melhorar ele."
  menu lista

calculaMediaImportancia :: [Item] -> Double
calculaMediaImportancia lista = 
  let somaImportancia = sum (map importancia lista)
      quantidadeItens = length lista
  in fromIntegral somaImportancia / fromIntegral quantidadeItens
