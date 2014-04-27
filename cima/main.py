""" Cima (Zulu for turn off)
"""
import sys
import pygame


class Game():
    """ This is to contain everything """
    def main(self, display):
        self.size = width, height = display.get_size()
        clock = pygame.time.Clock()
        speed = [10, 10]
        purple = 128, 0, 128

        sprites = pygame.sprite.Group()
        player = Player(sprites)
        self.walls = Walls(self.size)
        sprites.add(self.walls)

        while True:
            dt = clock.tick(30)
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    return
                if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                    return
                if event.type == pygame.VIDEORESIZE:
                    self.size = width, height = event.size
                    sprites.remove(self.walls)
                    self.walls.update(dt/1000, self)
                    sprites.add(self.walls)
                    display = pygame.display.set_mode(self.size, pygame.RESIZABLE)
                    display.fill(purple)

            sprites.update(dt/1000, self)
            display.fill(purple)
            sprites.draw(display)
            pygame.display.flip()


class Walls(pygame.sprite.Group):
    """ The borders of the game """
    def __init__(self, screen_size):
        super(Walls, self).__init__()
        self.create_walls(screen_size)

    def create_walls(self, screen_size):
        """ Places walls at the edges of the screen"""
        width, height = screen_size
        print(screen_size)
        super(Walls, self).update()
        block = pygame.image.load("ball.gif")
        block_height = 111
        for x in range(0, width, block_height):
            for y in range(0, height, block_height):
                if x in (0, width - block_height) or y in (0, height - block_height):
                    wall = pygame.sprite.Sprite(self)
                    wall.image = block
                    wall.rect = pygame.rect.Rect((x, y), block.get_size())

    def update(self, dt, game):
        """ Give the walls a turn"""
        print(self)
        self.empty()
        self.create_walls(game.size)


class Player(pygame.sprite.Sprite):
    """ Me """
    def __init__(self, *groups):
        super(Player, self).__init__(*groups)
        self.image = pygame.image.load("ball.gif")
        self.rect = self.image.get_rect()
        self.speed = [10, 10]

    def update(self, dt, game):
        """ Give the player a turn """
        key = pygame.key.get_pressed()
        if key[pygame.K_LEFT]:
            self.speed[0] += -2
        if key[pygame.K_RIGHT]:
            self.speed[0] += 4
        if key[pygame.K_UP]:
            self.speed[1] += -2
        if key[pygame.K_DOWN]:
            self.speed[1] += 4

        # if self.rect.left < 0 or self.rect.right > width:
        #     self.speed[0] = -self.speed[0]
        # if self.rect.top < 0 or self.rect.top > (height - ball.get_height()):
        #     self.speed[1] = -self.speed[1]

        if self.speed[0] > 10:
            self.speed[0] -= 1
        if self.speed[1] > 10:
            self.speed[1] -= 1

        self.rect = self.rect.move(self.speed)


if __name__ == '__main__':
    size = 777, 555
    pygame.init()
    display = pygame.display.set_mode(size, pygame.RESIZABLE)
    Game().main(display)
    sys.exit()
