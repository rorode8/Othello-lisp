# -*- coding: utf-8 -*-
"""
Created on Sat Mar 27 18:37:32 2021

@author: rorod
"""
import pygame
import pygame_menu
import random
import sys
import time
#constants
lst = [1]
BOARD_SIZE=483
WIDTH,HEIGHT=850, 483

diff = 1
print('cringe')
FPS = 60
WIN = pygame.display.set_mode((WIDTH,HEIGHT))
WIN.fill((255,255,255))
pygame.display.set_caption("Othello")
difficulty = {1:[3,'easy','Isabelle'], 2:[5,'medium','Peter'], 3:[7,'hard', 'Goku']}

def getRowCol(x,y):
    row = x//60
    col = y//60
    return row,col

def drawChip(row,col, fill,line):
    x=3+row*60+29
    y=3+col*60+29
    pygame.draw.circle(WIN,fill,(x,y),21,0)
    pygame.draw.circle(WIN,line,(x,y),22,3)

def drawEmptyCanvas():
    area = pygame.Rect(0,0,BOARD_SIZE,BOARD_SIZE)
    pygame.draw.rect(WIN,(0,0,0),area,0)
    for i in range(8):
        for j in range(8):
            rect =pygame.Rect(3+60*(i),3+60*(j),57,57)
            pygame.draw.rect(WIN,(38,148,38),rect,0)
            
     
    #OTHELLO implementation
def drawBoard(board):
    #
       # This function prints out the board that it was passed. Returns None.
    fill=(38,148,38)
    line = (38,148,38)
    for y in range(8):        
        for x in range(8):
            tile = board[x][y]
            line = (10,10,10)
            if(tile == 'O'):
                fill = (240,240,240)
            elif(tile == 'X'):
                fill = (25,25,25)
            else:
                continue
            drawChip(x,y,fill,line)
            
def resetBoard(board):
      # Blanks out the board it is passed, except for the original starting position.
    WIN.fill((255,255,255))
    drawEmptyCanvas()
    
    for x in range(8):
        for y in range(8):
            board[x][y] = ' '
      # Starting pieces:
    board[3][3] = 'X'
    board[3][4] = 'O'
    board[4][3] = 'O'
    board[4][4] = 'X'
     
def getNewBoard():
      # Creates a brand new, blank board data structure.
    board = []
    for i in range(8):
        board.append([' '] * 8)
    return board

def isOnBoard(x, y):
      # Returns True if the coordinates are located on the board.
    return x >= 0 and x <= 7 and y >= 0 and y <=7

def isValidMove(board, tile, xstart, ystart):
      # Returns False if the player's move on space xstart, ystart is invalid.
      # If it is a valid move, returns a list of spaces that would become the player's if they made a move here.
    if board[xstart][ystart] != ' ' or not isOnBoard(xstart, ystart):
        return False
    board[xstart][ystart] = tile # temporarily set the tile on the board.
    if tile == 'X':
        otherTile = 'O'
    else:
        otherTile = 'X'
    tilesToFlip = []
    for xdirection, ydirection in [[0, 1], [1, 1], [1, 0], [1, -1], [0, -1], [-1, -1], [-1, 0], [-1, 1]]:
        x, y = xstart, ystart
        x += xdirection # first step in the direction
        y += ydirection # first step in the direction
        if isOnBoard(x, y) and board[x][y] == otherTile:
               # There is a piece belonging to the other player next to our piece.
            x += xdirection
            y += ydirection
            if not isOnBoard(x, y):
                continue
            while board[x][y] == otherTile:
                x += xdirection
                y += ydirection
                if not isOnBoard(x, y): # break out of while loop, then continue in for loop
                    break
            if not isOnBoard(x, y):
                continue
            if board[x][y] == tile:
                  # There are pieces to flip over. Go in the reverse direction until we reach the original space, noting all the tiles along the way.
                while True:
                    x -= xdirection
                    y -= ydirection
                    if x == xstart and y == ystart:
                        break
                    tilesToFlip.append([x, y])
    board[xstart][ystart] = ' ' # restore the empty space
    if len(tilesToFlip) == 0: # If no tiles were flipped, this is not a valid move.
        return False
    return tilesToFlip       

def getValidMoves(board, tile):
    #
     # Returns a list of [x,y] lists of valid moves for the given player on the given board.
    validMoves = []
    for x in range(8):
       for y in range(8):
           if isValidMove(board, tile, x, y) != False:
               validMoves.append([x, y])
    return validMoves

def getBoardWithValidMoves(board, tile):
    #
      # Returns a new board with . marking the valid moves the given player can make.
    dupeBoard = getBoardCopy(board)
    for x, y in getValidMoves(dupeBoard, tile):
      dupeBoard[x][y] = '.'
    return dupeBoard

def getBoardCopy(board):
     # Make a duplicate of the board list and return the duplicate.
  dupeBoard = getNewBoard()
  for x in range(8):
      for y in range(8):
          dupeBoard[x][y] = board[x][y]
  return dupeBoard  

def getScoreOfBoard(board):
     # Determine the score by counting the tiles. Returns a dictionary with keys 'X' and 'O'.
    xscore = 0
    oscore = 0
    for x in range(8):
        for y in range(8):
            if board[x][y] == 'X':
                xscore += 1
            if board[x][y] == 'O':
                oscore += 1
    return {'X':xscore, 'O':oscore};

#REWORK!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
def enterPlayerTile():
     # Lets the player type which tile they want to be.
     # Returns a list with the player's tile as the first item, and the computer's tile as the second.
  tile = ''
  while not (tile == 'X' or tile == 'O'):
      print('Do you want to be X or O?')
      tile = input().upper()
     # the first element in the list is the player's tile, the second is the computer's tile.
  if tile == 'X':
      return ['X', 'O']
  else:
      return ['O', 'X']      
  
def whoGoesFirst():
     # Randomly choose the player who goes first.
  if random.randint(0, 1) == 0:
      return 'computer'
  else:
      return 'player'
def playAgain():
     # This function returns True if the player wants to play again, otherwise it returns False.
  print('Do you want to play again? (yes or no)')
  return input().lower().startswith('y')
def makeMove(board, tile, xstart, ystart):
     # Place the tile on the board at xstart, ystart, and flip any of the opponent's pieces.
     # Returns False if this is an invalid move, True if it is valid.
  tilesToFlip = isValidMove(board, tile, xstart, ystart)
  if tilesToFlip == False:
      return False
  board[xstart][ystart] = tile
  for x, y in tilesToFlip:
      board[x][y] = tile
  return True


def isOnCorner(x, y):
     # Returns True if the position is in one of the four corners.
  return (x == 0 and y == 0) or (x == 7 and y == 0) or (x == 0 and y == 7) or (x == 7 and y == 7)

#REWORK
def getPlayerMove(board, playerTile,x1,y1):
     # Let the player type in their move.
     # Returns the move as [x, y] (or returns the strings 'hints' or 'quit')
  DIGITS1TO8 = [0,1,2,3,4,5,6,7]
  while True:
      row,col = getRowCol(x1,y1)
      
      if row in DIGITS1TO8 and col in DIGITS1TO8:
          
          if isValidMove(board, playerTile, row, col) == False:
              return False
          else:
              break
      else:
          print('That is not a valid move. Type the x digit (1-8), then the y digit (1-8).')
          print('For example, 81 will be the top-right corner.')
          return False
  return [row, col]

def showPoints(playerTile, computerTile, mainBoard):
     # Prints out the current score.
  scores = getScoreOfBoard(mainBoard)
  return scores
  
def getComputerMove(board, computerTile):
     # Given a board and the computer's tile, determine where to
     # move and return that move as a [x, y] list.
  possibleMoves = getValidMoves(board, computerTile)
     # randomize the order of the possible moves
  random.shuffle(possibleMoves)
     # always go for a corner if available.
  for x, y in possibleMoves:
      if isOnCorner(x, y):
          return [x, y]
     # Go through all the possible moves and remember the best scoring move
  bestScore = -1
  for x, y in possibleMoves:
      dupeBoard = getBoardCopy(board)
      makeMove(dupeBoard, computerTile, x, y)
      score = getScoreOfBoard(dupeBoard)[computerTile]
      if score > bestScore:
          bestMove = [x, y]
          bestScore = score
  return bestMove

def drawScore(board, playerTile, pcTile, num):
    scores = showPoints(playerTile, pcTile, board)
    myfont = pygame.font.SysFont('Comic Sans MS', 30)
    myfont2 = pygame.font.SysFont('Comic Sans MS', 30)
    textsurface = myfont.render('Player: '+str(scores[playerTile]), False, (0, 0, 0))
    textsurface2 = myfont2.render('AI: '+str(scores[pcTile]), False, (0, 0, 0))
    rect =pygame.Rect(483, 0 ,WIDTH-483,50)
    pygame.draw.rect(WIN,(130,130,130),rect,0)
        
    rival = pygame.image.load(difficulty[num][2]+'.png').convert_alpha()
    
    WIN.blit(textsurface,(BOARD_SIZE+15,0))
    WIN.blit(textsurface2,(693,0))
    WIN.blit(rival, [BOARD_SIZE, HEIGHT-300]) 
    

    

def menu():
    num = 1
    
    def set_difficulty(value, difficulty):
        num = difficulty
        lst[0] = num
        print(num)
        

    def start_the_game():
        print('start')
        print(num)
        main(num)

        
    pygame.init()
    menu = pygame_menu.Menu(300, 400, 'Welcome',
                       theme=pygame_menu.themes.THEME_BLUE)

    
    menu.add.selector('Difficulty :', [('easy', 1), ('normal', 2), ('hard',3)], onchange=set_difficulty)
    menu.add.button('Play', start_the_game)
    menu.add.button('Quit', pygame_menu.events.EXIT)
    menu.mainloop(WIN)
    
    

def main(nums):
    
    diff = nums
    num = lst[0]
    pygame.font.init()
    run = True
    clock = pygame.time.Clock()
    # Reset the board and game.
    mainBoard = getNewBoard()
    resetBoard(mainBoard)
    playerTile, computerTile = 'X','O'
    turn = 'player'
    
    finish = False
    drawBoard(mainBoard)
    drawScore(mainBoard, playerTile, computerTile, num)
    print(diff)
    
    
    while run:
        ##Othello logic
        clock.tick(FPS)
        #print(turn)
        if(finish):
           for event in pygame.event.get():
               if event.type == pygame.KEYDOWN and event.key == pygame.K_r:
                    mainBoard = getNewBoard()
                    resetBoard(mainBoard)
                    
                    if playerTile == 'X':
                        playerTile, computerTile = 'O','X'
                        turn = 'computer'
                    else:
                        playerTile, computerTile = 'X','O'
                        turn = 'player'
                    
                    finish = False
                    drawBoard(mainBoard)
                    drawScore(mainBoard, playerTile, computerTile, num)
           
        else:
            
            if turn == 'computer':
                time.sleep(1)
                #showPoints(playerTile, computerTile)
                x, y = getComputerMove(mainBoard, computerTile)
                makeMove(mainBoard, computerTile, x, y)
                if getValidMoves(mainBoard, playerTile) == []:
                    
                    turn = 'computer'
                    if getValidMoves(mainBoard, computerTile) == []:
                        finish=True
                        
                else:
                    turn = 'player'
            
                drawBoard(mainBoard)
                drawScore(mainBoard, playerTile, computerTile, num)
        
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    run = False
                
                if event.type == pygame.KEYDOWN:
                    print('r')
                    
                if event.type == pygame.MOUSEBUTTONDOWN and turn == 'player':
                    print("x")
                    x,y = pygame.mouse.get_pos()
                    move = getPlayerMove(mainBoard, playerTile,x,y)
                    if(move!=False):
                        makeMove(mainBoard, playerTile, move[0], move[1])
                        turn = 'computer'
                    
                if getValidMoves(mainBoard, computerTile) == []:
                    turn = 'player'
                    if getValidMoves(mainBoard, playerTile) == []:
                        finish=True
                        
                
                    
                drawBoard(mainBoard)
                drawScore(mainBoard, playerTile, computerTile, num)
                
            
                
        
        pygame.display.update()
    
    while run:
        drawBoard(mainBoard)
        pygame.display.update()
    pygame.quit()
    
menu()
#main()