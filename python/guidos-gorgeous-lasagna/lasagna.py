"""Functions used in preparing Guido's gorgeous lasagna.

Learn about Guido, the creator of the Python language:
https://en.wikipedia.org/wiki/Guido_van_Rossum
"""

EXPECTED_BAKE_TIME = 40
PREPARATION_TIME = 2


def bake_time_remaining(elapsed_bake_time):
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int - baking time already elapsed.
    :return: int - remaining bake time (in minutes) derived from
    'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """
    return EXPECTED_BAKE_TIME - elapsed_bake_time


def preparation_time_in_minutes(layers):
    """Calculate preparation_time_in_minutes.

    :param layers: int - number of layers in the pie
    :return: int - time it should take to build the pie for baking

    Function that returns number of minutes it should take to build
    a pie of the specified number of layers.
    """
    return PREPARATION_TIME * layers


def elapsed_time_in_minutes(layers, so_far):
    """Calculate time spent in preparation plus bake time.

    :param layers: int - number of layers in the pie
    :param so_far: int - how many minutes the pie has been baking
    :return: int - how long since you started this pie.

    Function to calculate time spent on the pie.
    """
    return (layers * PREPARATION_TIME) + so_far
